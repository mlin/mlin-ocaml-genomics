open Batteries

type refseq = string
type t = string array
type ref2col = [`RefToCol of int array]
type col2ref = [`ColToRef of int array]

let sub_cols conseqs ~start ~length = Array.map (fun s -> String.sub s start length) conseqs

let refseq ?(row=0) conseqs =
	let conlen = String.length conseqs.(row)
	let refseq = String.create conlen
	let con2ref = Array.make conlen (-1)
	let ref2con = Array.make conlen (-1)
	let fst_consensus_nucleotide =
		let rec f j =
			let c = conseqs.(row).[j]
			if c = '-' || c = '.' then f (j+1) else j
		f 0
	let rec iter conj refj =
		if conj < conlen then
			ref2con.(refj) <- conj
			let cij = conseqs.(row).[conj]
			if cij = '-' || cij = '.' then
				con2ref.(conj) <- refj-1
				iter (conj+1) refj
			else
				con2ref.(conj) <- refj
				refseq.[refj] <- conseqs.(row).[conj];
				iter (conj+1) (refj+1)
		else
			(String.sub refseq 0 refj,(`RefToCol (Array.sub ref2con 0 refj)),(`ColToRef con2ref))
	for j = 0 to fst_consensus_nucleotide do
		con2ref.(j) <- 0
	((iter fst_consensus_nucleotide 0):(string*[`RefToCol of int array]*[`ColToRef of int array]))
	
let reverse =
	Array.map
		fun s ->
			let l = String.length s
			let s' = String.create l
			for i = 0 to l-1 do s'.[i] <- s.[l-i-1]
			s'
			
let map f =
	Array.map
		fun s ->
			let s' = String.create (String.length s)
			for i = 0 to String.length s - 1 do
				let c =	match s.[i] with
					| '-' -> '-' | '.' -> '.'
					| y -> f y
				s'.[i] <- c
			s'
			
let revmap f conseqs = reverse (map f conseqs)

let cols seqs =
	if Array.length seqs = 0 then invalid_arg "GenomeAlignment.Block.cols"
	Enum.init (String.length seqs.(0)) (fun j -> Array.init (Array.length seqs) (fun i -> seqs.(i).[j]))

let rm_gaps ?(row=0) seqs =
	let ar = Array.init (Array.length seqs) (fun _ -> Buffer.create 256)
	cols seqs |> iter
		fun col ->
			match col.(row) with
				| '-' | '.' -> ()
				| _ -> col |> Array.iteri (fun i ch -> Buffer.add_char ar.(i) ch)
	Array.map Buffer.contents ar

let which ar =
	let tbl = Hashtbl.create (Array.length ar)
	Array.iteri (fun i x -> Hashtbl.replace tbl x i) ar
	Hashtbl.find tbl
	
let remove_vacuous_columns seqs =
	let bufs = Array.init (Array.length seqs) (fun _ -> Buffer.create (String.length seqs.(0)))
	let any_col = ref false
	for j = 0 to (String.length seqs.(0)) - 1 do
		let any_row = ref false
		for i = 0 to Array.length seqs - 1 do if seqs.(i).[j] <> '-' then any_row := true
		if !any_row then
			any_col := true
			for i = 0 to Array.length seqs - 1 do Buffer.add_char bufs.(i) seqs.(i).[j]
	if not !any_col then failwith "GenomeAlignment.Block.concat: all-gap alignment"
	Array.map Buffer.contents bufs

module StringTopSort = TopSort.Make(String)
type names = string array
let stitch ?(missing_char='-') ?order alns =
	let order = match order with
		| Some ar -> ar
		| None ->
			let all_ids = List.fold_right StringTopSort.Items.union (List.map (fun (ids,_) -> Array.fold_right StringTopSort.Items.add ids StringTopSort.Items.empty) alns) StringTopSort.Items.empty
			let constraints = List.map (fun (ids,_) -> StringTopSort.partial_order (Array.to_list ids)) alns
			try
				Array.of_list (StringTopSort.sort all_ids (List.fold_right StringTopSort.Constraints.union constraints StringTopSort.Constraints.empty))
			with
				| StringTopSort.Unsat rows ->
					let rows = String.concat " " (StringTopSort.Items.elements rows)
					failwith (Printf.sprintf "GenomeAlignment.Block.concat: two or more of the following rows are not in consistent order: %s" rows)
	let tot_conlen =
		List.fold_left (+) 0
			List.map
				fun (_,conseqs) ->
					let conlen = String.length conseqs.(0)
					Array.iter (fun s -> if String.length s <> conlen then failwith "Block.concat: found a block with inconsistent sequence lengths") conseqs
					conlen
				alns
	let spliced_conseqs = Array.init (Array.length order) (fun _ -> Buffer.create tot_conlen)
	List.iter
		fun (ids,conseqs) ->
			let missing = lazy (String.make (String.length conseqs.(0)) missing_char)
			let which_id = which ids
			Array.iteri
				fun i id ->
					try
						Buffer.add_string spliced_conseqs.(i) conseqs.(which_id id)
					with
						| Not_found -> Buffer.add_string spliced_conseqs.(i) (Lazy.force missing)
				order
		alns
	order, remove_vacuous_columns (Array.map Buffer.contents spliced_conseqs)

let input_mfa input =
	try
		let lines = IO.lines_of input
		let rec seq sofar =
			let buf = match sofar with ((_,buf) :: _) -> buf | _ -> assert false
			if not (Enum.is_empty lines) then
				let line = String.trim (Option.get (peek lines))
				if String.length line = 0 then
					junk lines
					seq sofar
				else if line.[0] <> '>' then
					Buffer.add_string buf line
					junk lines
					seq sofar
				else
					hdr sofar
			else
				let enum = List.rev sofar |> Array.of_list
				let species = Array.map fst enum
				let seqs = Array.map (fun (_,buf) -> Buffer.contents buf) enum
				let seqlen = String.length seqs.(0)
				if seqlen = 0 || exists ((=) "") (Array.enum species) || exists (fun s -> String.length s <> seqlen) (Array.enum seqs) then
					failwith "empty species name or sequence, or sequence length mismatch"
				species, seqs
		and hdr sofar =
			let line = Option.get (get lines)
			if String.length line = 0 then
				hdr sofar
			else if line.[0] <> '>' then
				failwith "bad header"
			else
				let hdr = String.lchop line
				let sp = String.trim (try String.left hdr (String.index hdr '|') with Not_found -> hdr)
				seq ((sp,Buffer.create 256) :: sofar)
		hdr []
	with
		| Failure msg -> failwith ("Alignment.Block.input_mfa: " ^ msg)
		| exn -> failwith ("invalid MFA alignment: " ^ (Printexc.to_string exn))
