open Batteries_uni
open OptParse
open Printf
open Genomics.Annotation

let opt_parser = OptParser.make ~usage:"%prog source.gxfdb destination.gxfdb\nCopy annotations from source to destination, but include only the longest within any cluster of overlapping gene models" ()
let opt ?group ?h ?hide ?s ?short_names ?l ?long_names x = OptParser.add opt_parser ?group ?help:h ?hide ?short_name:s ?long_name:l x; x

let index = opt ~l:"no-index" ~h:"import GxF but do not create/update database indices" (StdOpt.store_false ())

let fn_src, fn_dest = match OptParser.parse_argv opt_parser with
	| [ fn_src; fn_dest ] -> fn_src, fn_dest
	| _ ->
		OptParser.usage opt_parser ()
		exit (-1)
		
module Tx = struct
	type t = {
		nt : int;
		lo : int;
		hi : int;
		strand : [`Plus | `Minus];
		id : string;
		features : GxF.feature list
	}
	let lo { lo } = lo
	let hi { hi } = hi
module TxI = Genomics.IntervalOps.Make(Tx)

GxF.DB.with_db fn_src
	fun src_db ->
		GxF.DB.with_db fn_dest
			fun dest_db ->
				GxF.DB.transact dest_db
					fun _ -> 
						GxF.DB.seqids src_db |> List.iter
							fun seqid ->
								let make_tx features =
									let exons = List.filter (fun { GxF.ty } -> ty = "exon") features
									let exonsp, exonsm = List.partition (function { GxF.strand = `minus } -> false | _ -> true) exons
									let strand = if List.length exonsp >= List.length exonsm then `Plus else `Minus
									{ Tx.nt = List.fold_left (fun tot { GxF.lo; hi } -> tot + hi - lo + 1) 0 exons;
									  lo = List.fold_left (fun cur { GxF.lo } -> min cur lo) max_int features;
									  hi = List.fold_left (fun cur { GxF.hi } -> max cur hi) min_int features;
									  strand = strand;
									  id = (List.hd features).GxF.id;
									  features = features }
								let txs = ((GxF.DB.seqid_ids src_db seqid |> List.enum)
				        				   /@ GxF.DB.query_id src_db /@ make_tx
				        				   |> List.of_enum |> TxI.sort)
								let txsp, txsm = List.partition (function { Tx.strand = `Plus } -> true | _ -> false) txs
								((TxI.cluster txsp) @ (TxI.cluster txsm)) |> List.iter
									fun cluster ->
										let tx = List.max cluster
										tx.Tx.features |> List.iter (GxF.DB.insert dest_db)
				if Opt.get index then GxF.DB.analyze dest_db
