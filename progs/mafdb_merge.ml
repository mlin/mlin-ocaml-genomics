open Batteries_uni
open OptParse
open Printf
open Sqlite3EZ
open Genomics.Alignment
module StringSet = Set.StringSet

let opt_parser = OptParser.make ~usage:"%prog destination.mafdb source1.mafdb source2.mafdb ..." ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let delete = opt ~l:"delete-sources" ~s:'d' ~h:"delete source databases once they have been successfully merged" (StdOpt.store_true ())
let analyze = opt ~l:"no-analyze" ~h:"do not update destination indices" (StdOpt.store_false ())
let check_overlap = opt ~l:"no-check-overlap" ~h:"do not check if the destination already contains blocks from the same sources" (StdOpt.store_false ())

let cmd = OptParser.parse_argv opt_parser

if List.length cmd < 2 then
	OptParser.usage opt_parser ()
	exit (-1)
	
let fn_dest = List.hd cmd
let fns_dbs = (List.tl cmd) |> List.enum

let is_mafdb fn =
	if not (Sys.file_exists fn) then
		false
	else
		try
			let dbc = db_open fn
			() |> finally
				fun () -> ignore (db_close dbc)
				fun _ ->
					statement_query
						make_statement dbc "SELECT name FROM sqlite_master WHERE type='table' AND name='MAFDB'"
						[||]
						fun _ -> true
						(||)
						false
		with exn -> false

let sources fn =
	if not (is_mafdb fn) then invalid_arg "not a MAFDB, cannot get sources"
	let mafdb = MAF.DB.db_open fn
	MAF.DB.(finally (fun () -> db_close mafdb) (fun () -> sources mafdb) ())

MAF.DB.with_db fn_dest (fun _ -> ())
assert (is_mafdb fn_dest)
let dest_sources = ref (StringSet.of_enum (List.enum (sources fn_dest)))
let needs_analysis = ref false

Sqlite3EZ.with_db fn_dest
	fun db ->
		Sqlite3EZ.transact db
			fun db ->
				fns_dbs |> iter
					fun fn_db ->
						eprintf "%s..." fn_db
						flush stderr
						if not (is_mafdb fn_db) then
							eprintf "not a MAFDB, skipping!\n"
							flush stderr
						else
							let db_sources = StringSet.of_enum (List.enum (sources fn_db))
							let conflicts = StringSet.inter !dest_sources db_sources
							if Opt.get check_overlap && StringSet.cardinal conflicts > 0 then
								failwith ("destination already contains these source sequences: " ^ (String.concat " " (List.of_enum (StringSet.enum conflicts))))
							dest_sources := StringSet.union db_sources !dest_sources

							exec db (sprintf "ATTACH '%s' AS SOURCE" fn_db)
							exec db "INSERT INTO MAFDB(src,src_start_0inc,src_end_0inc,block) SELECT src,src_start_0inc,src_end_0inc,block FROM SOURCE.MAFDB"
							needs_analysis := true
							exec db "DETACH SOURCE"
							if Opt.get delete then ignore (Sys.command ("rm -f " ^ fn_db))
							eprintf "OK\n"
							flush stderr


if Opt.get analyze && !needs_analysis then
	eprintf "updating indices...\n"
	flush stderr
	MAF.DB.(with_db fn_dest (fun db -> transact db (fun _ -> analyze db)))
