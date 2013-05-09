(* TODO rework to use WGA *)

open Batteries
open Extlib.OptParse
open Printf
open Genomics

let opt_parser = OptParser.make ~usage:"%prog hg19.wgadb hg19.chr22 1234 2345\ncoordinates are one-based, inclusive." ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x
let extant_file ?metavar () = Opt.value_option  "/FILE" None (fun fn -> if not (Sys.file_exists fn) || Sys.is_directory fn then raise Not_found; fn) (fun _ _ -> "file not found")

let assembly_map = opt ~l:"map" ~h:"assembly map" (StdOpt.str_option ())

let cmd = OptParser.parse_argv opt_parser

let fn_wgadb, ref_asmbl, chrom, lo, hi = match cmd with
	| [ one; two ] ->
		let ref_asmbl, chrom, lo, hi = Position.of_string two
		one, ref_asmbl, chrom, lo, hi
	| [one; two; three; four ] ->
		one, "", two, (int_of_string three), (int_of_string four)
	| _ ->
		OptParser.usage opt_parser ()
		exit (-1)
	
Alignment.WGA.with_db ?assembly_map:(Opt.opt assembly_map) ~lockless:true fn_wgadb
	fun wgadb ->
		let pos = (ref_asmbl,chrom,lo,hi)
		let sps, aln = Alignment.WGA.query_position wgadb pos
		for i = 0 to Array.length sps - 1 do
			printf ">%s\n" sps.(i)
			print_string aln.(i)
			print_char '\n'
