open Batteries_uni
open OptParse
open Printf
open Genomics.Alignment

let opt_parser = OptParser.make ~usage:"%prog alignments.mafdb alignment1.maf [alignment2.maf ...]\nMAF files can be .maf or .maf.gz" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let desired_src = opt ~l:"src" ~h:"import only those blocks with this as the first sequence, e.g. 'hg19.chr22'. If unspecified, use all blocks." (StdOpt.str_option ())
let check_strand = opt ~l:"allowRevComp" ~h:"permit insertion of a block representing the first sequence in reverse-complemented orientation" (StdOpt.store_false ())
let analyze = opt ~l:"no-analyze" ~h:"do not update indices" (StdOpt.store_false ())

let cmd = OptParser.parse_argv opt_parser

if List.length cmd < 2 then
	OptParser.usage opt_parser ()
	exit (-1)
	
let fn_mafdb = List.hd cmd
let fns_mafs = (List.tl cmd) |> List.enum

let n = ref 0
let tm0 = ref (Unix.gettimeofday ())

let process_maf db fn_maf =
	let input =	if Filename.check_suffix fn_maf ".gz" then Unix.open_process_in ("gunzip -c " ^ fn_maf) else File.open_in fn_maf
	() |> finally (fun _ -> close_in input)
		fun _ ->
			foreach (MAF.parse_minimally input)
				fun block ->
					try
						let { MAF.src; start; size; strand } = block.MAF.sequences.(0)

						if (match Opt.opt desired_src with None -> true | Some src' when src = src' -> true | _ -> false) then
							if Opt.get check_strand && strand <> `plus then
								failwith "Block represents the first sequence in reverse-complemented orientation"
							
							MAF.DB.insert db block
							
							incr n
							if !n mod 10000 = 0 then
								let tm = Unix.gettimeofday ()
								let secs = tm -. !tm0
								printf "%d insertions completed, %.2f insertions/sec\n" !n (10000. /. secs)
								flush stdout
								tm0 := tm
					with
						| exn ->
							eprintf "exception while processing the following block:\n%s\n" block.MAF.unparsed
							flush stderr
							raise exn

MAF.DB.with_db fn_mafdb
	fun mafdb ->
		MAF.DB.transact mafdb (fun _ -> iter (process_maf mafdb) fns_mafs)
		printf "Committed %d blocks.\n" !n

		if Opt.get analyze then
			printf "Analyzing...\n"
			flush stdout
			MAF.DB.(transact mafdb (fun _ -> analyze mafdb))
