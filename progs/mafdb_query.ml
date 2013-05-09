open Batteries
open Extlib.OptParse
open Printf
open Genomics.Alignment

let opt_parser = OptParser.make ~usage:"%prog alignments.mafdb hg19.chr22 1234 2345\ncoordinates are one-based, inclusive" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let list_sources = opt ~s:'l' ~l:"list-sources" ~h:"list all source sequences in database" (StdOpt.store_true ())

let cmd = OptParser.parse_argv opt_parser

if List.length cmd < 1 then
	OptParser.usage opt_parser ()
	exit (-1)

let fn_mafdb = List.hd cmd
let dbc = MAF.DB.db_open fn_mafdb

if Opt.get list_sources then
	MAF.DB.sources dbc |> List.enum |> iter print_endline
	exit 0

let coord str = int_of_string (String.replace_chars (function ',' -> "" | c -> String.of_char c) str)

let src,lo,hi =
	try
		List.nth cmd 1, (coord (List.nth cmd 2)), (coord (List.nth cmd 3))
	with
		| exn ->
			OptParser.usage opt_parser ()
			exit (-1)

MAF.(foreach (List.enum (DB.query dbc src lo hi)) (fun block -> print_endline block.unparsed))
