open Batteries_uni
open OptParse
open Printf
open Genomics

let opt_parser = OptParser.make ~usage:"%prog genome.db chr1.fa [chr2.fa ...]\nSource sequence files can be .fa or .fa.gz, and may contain multiple sequences" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let prefix = opt ~l:"prefix" ~h:"add this prefix to sequence names" (StdOpt.str_option ~default:"" ())
let fragment_size = opt ~l:"fragment-size" ~h:"size of individual sequence fragments (default 8,192)" (StdOpt.int_option ~default:8192 ())

let cmd = OptParser.parse_argv opt_parser

if List.length cmd < 2 then
	OptParser.usage opt_parser ()
	exit (-1)
	
let fn_db = List.hd cmd
let fns_sequences = (List.tl cmd) |> List.enum

let process_sequences db fn_sequences =
	let input =	if Filename.check_suffix fn_sequences ".gz" then Unix.open_process_in ("gunzip -c " ^ fn_sequences) else File.open_in fn_sequences
	() |> finally (fun _ -> close_in input)
		fun _ ->
			let lines = IO.lines_of input
			let rec outerloop () =
				match Enum.get lines with
					| None -> ()
					| Some line when String.length line > 0 && line.[0] = '>' ->
						let src = String.trim (String.sub line 1 (String.length line - 1))
						if src = "" then failwith "invalid sequence header"
						let src = Opt.get prefix ^ src
						eprintf "%s\n" src
						flush stderr
						
						let buf = Buffer.create (2 * Opt.get fragment_size)
						let rec innerloop ofs =
							match Enum.peek lines with
								| None ->
									if Buffer.length buf > 0 then
										Sequence.DB.insert db src (ofs+1) (Buffer.contents buf)
										Buffer.clear buf
								| Some line when String.length line > 0 && line.[0] = '>' ->
									if Buffer.length buf > 0 then
										Sequence.DB.insert db src (ofs+1) (Buffer.contents buf)
										Buffer.clear buf
								| Some line ->
									Enum.junk lines
									Buffer.add_string buf (String.trim line)
									let ofs = ref ofs
									while Buffer.length buf >= Opt.get fragment_size do
										let fgtsz = Opt.get fragment_size
										let fgt = Buffer.sub buf 0 fgtsz
										assert (String.length fgt = fgtsz)
										let rem = Buffer.sub buf fgtsz (Buffer.length buf - fgtsz)
										Sequence.DB.insert db src (!ofs+1) fgt
										Buffer.clear buf
										Buffer.add_string buf rem
										ofs := !ofs + fgtsz
									innerloop !ofs
									
						innerloop 0
						outerloop ()
					| _ -> failwith "missing/invalid sequence header"
			outerloop ()			

Sequence.DB.with_db fn_db
	fun db ->
		Sequence.DB.transact db (fun _ -> iter (process_sequences db) fns_sequences)
		Sequence.DB.(transact db (fun _ -> analyze db))
