#! /usr/bin/env ocamlscript
Ocaml.packs := ["Genomics"; "CamlPaml"];;
Ocaml.pp :=	Some (fun file -> let o = (Filename.chop_extension file) ^ ".pp.ml" in Pipeline.command ["ocaml+twt"; file; "-o"; o ], [o]);;
Ocaml.ocamlflags := ["-g"]
--
open Batteries_uni
open OptParse
open CamlPaml
open Sqlite3EZ
open Printf

let opt_parser = OptParser.make ~usage:"%prog hg19.wgadb [Species1 Species2[=Name2] ...]\nImport a WGA assembly map based on the Newick tree (nh) files provided with UCSC's MULTIZ alignments" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let map_name = opt ~l:"name" ~h:"assembly map name" (StdOpt.str_option ~default:"default" ())
let fn_db_nh = opt ~l:"ucscDbNames" ~h:"Newick tree with UCSC database names" (StdOpt.str_option ())
let fn_common_nh = opt ~l:"commonNames" ~h:"Newick tree with common names, isomorphic to DB names tree" (StdOpt.str_option ())

let cmd = OptParser.parse_argv opt_parser

let fn_wga, sps_include = match cmd with
	| fn_wga :: sps_include when Sys.file_exists fn_wga -> fn_wga, List.enum sps_include
	| _ -> 
		OptParser.usage opt_parser ()
		exit (-1)
		
if Opt.is_set fn_common_nh && not (Opt.is_set fn_db_nh) then
	OptParser.usage opt_parser ()
	exit (-1)

let load_tree fn = T.of_newick (NewickParser.parse NewickLexer.token (Lexing.from_input (open_in fn)))
let t_db = Option.map load_tree (Opt.opt fn_db_nh)
let t_common = Option.map load_tree (Opt.opt fn_common_nh)

match t_db, t_common with
	| Some t, Some t' when not (T.congruent ~labels:false ~branches:false t t') ->
		failwith "--ucscDbNames and --commonNames must specify isomorphic trees"
	| _ -> ()
	
let find_label t lbl =
	let rec loop i =
		if i >= T.leaves t then
			raise Not_found
		else if T.label t i = lbl then
			i
		else
			loop (i+1)
	loop 0

let assembly_map =
	if Enum.is_empty sps_include then
		match t_db with
			| None ->
				OptParser.usage opt_parser ()
				exit (-1)
			| Some t ->
				(0 -- (T.leaves t - 1)) |> map
					fun i ->
						let asmbl = T.label t i
						let name = Option.map (fun t' -> T.label t' i) t_common
						asmbl, name
	else
		sps_include |> map
			fun item ->
				try
					let asmbl, name = String.split item "="
					if String.length asmbl > 0 && String.length name > 0 then
						asmbl, (Some name)
					else
						printf "could not understand: %s\n" item
						OptParser.usage opt_parser ()
						exit (-1)
				with
					| Not_found ->
						try
							match t_db, t_common with
								| Some t, Some t' ->
									let lf = try find_label t item with Not_found -> find_label t' item
									T.label t lf, Some (T.label t' lf)
								| _ -> item, None
						with Not_found -> item, None

let rec prompt_yn ?(prompt="(Y/N)? ") ?default () =
	if not (Unix.isatty Unix.stdout) then
		match default with
			| None -> failwith "prompt_yn: not a tty and no default specified"
			| Some rslt -> rslt
	else
		print_string prompt
		flush stdout
		match String.uppercase (input_line stdin) with
			| "Y" | "YES" -> true
			| "N" | "NO" -> false
			| _ -> prompt_yn ~prompt ?default ()

with_db fn_wga
	fun db ->
		try
			transact db
				fun _ ->
					exec db "CREATE TABLE IF NOT EXISTS AssemblyMap(map TEXT NOT NULL,ord INTEGER NOT NULL,assembly TEXT NOT NULL,name TEXT);\
CREATE INDEX IF NOT EXISTS AssemblyMap_map ON AssemblyMap(map)"
					statement_exec (make_statement db "DELETE FROM AssemblyMap WHERE map=?")
						[| Data.TEXT (Opt.get map_name) |]
					let insert = make_statement db "INSERT INTO AssemblyMap VALUES(?,?,?,?)"
					let ord = ref 1
					assembly_map |> iter
						fun (asmbl, name) ->
							printf "%s\t%d\t%s\t%s\n"
								Opt.get map_name
								!ord
								asmbl
								Option.default "" name
							statement_exec insert [| Data.TEXT (Opt.get map_name);
							                         Data.INT (Int64.of_int !ord);
						                             Data.TEXT asmbl;
													 Option.map_default (fun x -> Data.TEXT x)
													     Data.NULL name |]
							incr ord

					if not (prompt_yn ~prompt:"COMMIT (Y/N)? " ~default:true ()) then raise Exit
		with Exit -> ()
