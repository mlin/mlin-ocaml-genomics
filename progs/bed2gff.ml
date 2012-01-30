#! /usr/bin/env ocamlscript
Ocaml.packs := ["batteries"; "GxF"];;
Ocaml.pp :=     Some (fun file -> let o = (Filename.chop_extension file) ^ ".pp.ml" in Pipeline.command ["ocaml+twt"; file; "-o"; o ], [o]);;
Ocaml.ocamlflags := ["-g"]
--
open Batteries_uni;; open OptParse;; open Printf

let opt_parser = OptParser.make ~usage:"%prog [annotations1.bed annotations2.bed[.gz] ...]\nIf no filenames are specified, stdin is used." ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let fn_output = opt ~s:'o' ~h:"output filename (if not specified, stdout is used)" (StdOpt.str_option ())
let source = opt ~l:"source" ~h:"what to put for the \"source\" column of GFF file (default \"bed2gff\")" (StdOpt.str_option ~default:"bed2gff" ())
let ty = opt ~l:"type" ~h:"what to put for the \"type\"/\"category\" column of the GFF file (default \"exon\")" (StdOpt.str_option ~default:"exon" ())

let one_based = opt ~l:"zero-based" ~h:"do not add one to BED coordinates (as BED is zero-based and GFF is one-based, this is usually desirable)" (StdOpt.store_false ())

(* TODO
let thick = opt ~l:"thick" ~h:"output separate features for \"thick\" regions specified by the BED file" (StdOpt.store_true ())
let thickType = opt ~l:"thick-type" ~h:"what to put for the \"type\"/\"category\" column for \"thick\" features (default \"CDS\")" (StdOpt.str_option ~default:"CDS" ())
*)

let fns_beds = OptParser.parse_argv opt_parser |> List.enum

let inputs =
	if Enum.is_empty fns_beds then
		Enum.singleton stdin
	else
		fns_beds |> map (fun fn -> if Filename.check_suffix fn ".gz" then Unix.open_process_in ("gunzip -c " ^ fn) else File.open_in fn)
let output = Option.default stdout (Option.map (fun fn -> File.open_out fn) (Opt.opt fn_output))

let parse_bed line =
	let flds = String.nsplit line "\t" |> Array.of_list
	if Array.length flds < 4 then failwith "too few fields"
	
	let seqid = flds.(0)
	let lo = int_of_string flds.(1)
	let hi = int_of_string flds.(2)
	let id = flds.(3)
	
	let score = if Array.length flds > 4 then Some (float_of_string flds.(4)) else None
	let strand = if Array.length flds > 5 then (match flds.(5) with "+" -> `plus | "-" -> `minus | "." -> `none | "*" | "?" -> `unknown | _ -> failwith "invalid strand") else `unknown
	
	if Array.length flds <> 12 then
		if hi > lo then
			Enum.singleton { GxF.seqid = seqid; lo = (lo+1); hi = hi;
							source = Opt.get source; ty = Opt.get ty;
							score = score; strand = strand; phase = None;
							id = id; attributes = []; unparsed = None }
		else
			eprintf "Warning: ignoring zero-length feature %s\n" id
			flush stderr
			Enum.empty ()
	else
		let intlst str = String.nsplit str "," |> List.filter_map (function "" -> None | s -> Some s) |> List.map int_of_string
		let blockSizes = intlst flds.(10)
		let blockStarts = intlst flds.(11)
		List.combine blockSizes blockStarts |> List.enum |> Enum.filter_map
			fun (len,ofs) ->
				if len > 0 then
					let blocklo = lo+ofs+1
					let blockhi = lo+ofs+len
					Some { GxF.seqid = seqid; lo = blocklo; hi = blockhi;
						source = Opt.get source; ty = Opt.get ty;
						score = score; strand = strand; phase = None;
						id = id; attributes = []; unparsed = None }
				else
					eprintf "Warning: ignoring zero-length block in %s\n" id
					flush stderr
					None

inputs |> iter
	fun input ->
		() |> finally (fun _ -> close_in input)
			fun _ ->
				IO.lines_of input |> iter
					fun line ->
						try
							if String.length line > 0 && line.[0] <> '#' then
								line |> parse_bed |> map GxF.feature_to_string |> iter (fprintf output "%s\n")
						with
							| exn ->
								eprintf "%s\nError processing the preceding line: " line
								raise exn
