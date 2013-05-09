open Batteries
open Extlib.OptParse
open Printf
open Genomics.Annotation

let opt_parser = OptParser.make ~usage:"%prog annotations.db [annotations1.gtf annotations2.gxf[.gz] ...]\nIf no GxF filenames are specified, stdin is used." ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let id_att = opt ~l:"id" ~h:"override default choice of ID attribute for each feature" (StdOpt.str_option ())
let digest_alias = opt ~l:"digest" ~h:"make digest aliases" (StdOpt.store_true ())
let just_index = opt ~l:"just-index" ~h:"do not necessarily expect any GxF input, but still create/update database indices" (StdOpt.store_true ())
let index = opt ~l:"no-index" ~h:"import GxF but do not create/update database indices" (StdOpt.store_false ())
let verbose = opt ~s:'v' ~h:"print status messages" (StdOpt.store_true ())

let cmd = OptParser.parse_argv opt_parser

if List.length cmd < 1 || (Opt.get just_index && not (Opt.get index)) then
	OptParser.usage opt_parser ()
	exit (-1)
	
let fn_db = List.hd cmd
let fns_sequences = (List.tl cmd) |> List.enum

let choose_id =
	(Opt.opt id_att) |> Option.map
		fun key keys_values ->
			try
				List.assoc key keys_values
			with
				| Not_found -> failwith "could not find specified ID attribute"

let ensure_unparsed ?format_attributes ft =
	match ft.GxF.unparsed with
		| Some _ -> ft
		| None -> { ft with GxF.unparsed = Some (GxF.feature_to_string ?format_attributes ft) }

GxF.DB.with_db fn_db
	fun db ->
		let n = ref 0
		let incr_status () =
			incr n
			if Opt.get verbose && !n mod 50000 = 0 then
				eprintf "%d...\n" !n
				flush stderr
		GxF.DB.transact db
			fun _ -> 
				let inputs =
					if Enum.is_empty fns_sequences then
						if Opt.get just_index then Enum.empty () else Enum.singleton stdin
					else
						fns_sequences |> map (fun fn -> if Filename.check_suffix fn ".gz" then Unix.open_process_in ("gunzip -c " ^ fn) else File.open_in fn)

				inputs |> iter
					fun input ->
						() |> finally
							fun _ -> close_in input
							fun _ ->
								IO.lines_of input |> iter
									fun line ->
										try
											if String.length line > 0 && line.[0] <> '#' then
												let ft = ensure_unparsed (GxF.parse_feature ?choose_id line)
												if GxF.(ft.hi < ft.lo) then failwith "feature hi < lo"
												let aliases =
													if Opt.get digest_alias then
														Some [(Digest.to_hex (Digest.string (Option.get ft.GxF.unparsed)))]
													else
														None
												GxF.DB.insert ?aliases db ft
												incr_status ()
										with
											| exn ->
												eprintf "%s\nexception processing the preceding line: %s\n" line (Printexc.to_string exn)
												flush stderr
		if Opt.get verbose then
			eprintf "Finished importing %d features. They have been committed.\n" !n
			flush stderr 

		if Opt.get index then
			if Opt.get verbose then
				eprintf "Indexing...\n"
				flush stderr
			GxF.DB.(transact db (fun _ -> analyze db))
