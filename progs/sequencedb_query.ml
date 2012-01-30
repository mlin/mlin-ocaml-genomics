open Batteries_uni
open OptParse
open Printf
open Genomics

let opt_parser = OptParser.make ~usage:"%prog genome.db hg19.chr22 1234 2345\ncoordinates are one-based, inclusive" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let list_sources = opt ~s:'l' ~l:"list-sequences" ~h:"list all source sequences in database" (StdOpt.store_true ())

let cmd = OptParser.parse_argv opt_parser

if List.length cmd < 1 then
	OptParser.usage opt_parser ()
	exit (-1)

let fn_db = List.hd cmd
let dbc = Sequence.DB.db_open fn_db

if Opt.get list_sources then
	Sequence.DB.ids dbc |> List.iter print_endline
	exit 0

let coord str = int_of_string (String.replace_chars (function ',' -> "" | c -> String.of_char c) str)

let src,lo,hi =
	try
		List.nth cmd 1, (coord (List.nth cmd 2)), (coord (List.nth cmd 3))
	with
		| exn ->
			OptParser.usage opt_parser ()
			exit (-1)

print_endline (Sequence.DB.query dbc src lo hi)
