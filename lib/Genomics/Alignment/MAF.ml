open Batteries_uni

include MAF_Defs

(* break the input down into "paragraphs" separated by blank lines, stripping out any comments in between. *)
let paragraphs input =
	let lines = IO.lines_of input
	Enum.from
		fun () ->
			let rec skip_blank () =
				match Enum.peek lines with
					| None -> raise Enum.No_more_elements
					| Some line ->
						if (String.length (String.trim line) = 0 ||
							(String.length line > 0 && line.[0] = '#')) then
							Enum.junk lines
							skip_blank ()
			skip_blank ()
			let para = Buffer.create 256
			let rec input_block () =
				match Enum.get lines with
					| None -> ()
					| Some line ->
						if String.length line > 0 then
							Buffer.add_string para line
							Buffer.add_char para '\n'
							input_block ()
			input_block ()
			if Buffer.length para = 0 then raise Enum.No_more_elements
			Buffer.contents para

let block_of_paragraph ?(minimal=false) txt =
	let block = (if minimal then MAF_Block_Parser.parse_minimally
					else MAF_Block_Parser.parse) MAF_Block_Lexer.token (Lexing.from_string txt)
	assert (Array.length block.sequences > 0)
	let len = String.length block.sequences.(0).text
	if block.sequences |> Array.enum |> exists (fun { text } -> String.length text <> len) then
		failwith "inconsistent sequence lengths"
	{ block with unparsed = txt }

let parse input = Enum.map block_of_paragraph (paragraphs input)

let parse_minimally input = Enum.map (block_of_paragraph ~minimal:true) (paragraphs input)

let stitch ?(src_transform=identity) ?missing_char ?refseq ?order ~src ~lo ~hi blocks =
	if hi < lo then invalid_arg "Alignment.MAF.stitch: hi < lo"

	let to_splice = ref []
	let pos = ref lo
	let refseq_mismatches = ref 0
		
	List.iter
		fun block ->
			let block_lo = block.sequences.(0).start
			let breflen = block.sequences.(0).size
			let block_hi = block_lo + breflen - 1
			
			(* Printf.printf "%d %d %d %d %d %s\n" lo block_lo block_hi reflen (Array.length block.sequences) block.sequences.(0).src *)
			
			if block_hi >= !pos && block_lo <= hi then
				let ids = Array.map src_transform (block_sources block)
				if ids.(0) <> src_transform src then
					invalid_arg (Printf.sprintf "Alignment.MAF.stitch: block with wrong reference src (expected '%s', got '%s')" ids.(0) (src_transform src))
				if block_lo > !pos then
					match refseq with
						| None -> failwith "Alignment.MAF.stitch: discontiguity"
						| Some refseq ->
							if String.length refseq <> hi-lo+1 then
								failwith "MAF.assemble: inappropriate reference sequence length"
							let piece = String.sub refseq (!pos - lo) (block_lo - !pos)
							to_splice := ([|src_transform src|],[|piece|]) :: !to_splice
							pos := block_lo
				else if !pos > lo && block_lo < !pos then
					failwith "Alignment.MAF.stitch: detected overlap"
				let conseqs = MAF_Defs.block block
				let brefseq,(`RefToCol ref2con),_ = Block.refseq conseqs
				if String.length brefseq <> breflen then
					failwith "Alignment.MAF.stitch: mismatch between block sequence size and actual reference length"

				(* truncate conseqs to beginning/end of desired region, if necessary *)
				let desired_lo = !pos - block_lo
				let desired_hi = min (hi-block_lo) (breflen-1)
				let desired_length = desired_hi-desired_lo+1
				(*Printf.printf "%d %d %d\n" desired_lo desired_hi desired_length*)
				let conseqs' =
					if desired_lo > lo || desired_hi < block_hi then
						let desired_conlo = if desired_lo > 0 then ref2con.(desired_lo) else 0
						let desired_conhi =
							if desired_hi < breflen-1 then
								ref2con.(desired_hi)
							else
								String.length conseqs.(0) - 1
						Block.sub_cols conseqs desired_conlo (desired_conhi - desired_conlo + 1)
					else
						conseqs
				
				refseq |> Option.may
					fun refseq ->
						for i = desired_lo to desired_hi do
							let blockch = Char.uppercase brefseq.[i]
							let conch = Char.uppercase conseqs.(0).[ref2con.(i)]
							let refch = Char.uppercase refseq.[!pos + i - desired_lo - lo]
							if blockch <> conch || (blockch <> 'N' && refch <> 'N' &&
														blockch <> refch) then
								incr refseq_mismatches

				to_splice := (ids,conseqs') :: !to_splice
				pos := block_lo+desired_lo+desired_length
		List.sort ~cmp:(fun b1 b2 -> compare b1.sequences.(0).start b2.sequences.(0).start) blocks
		
	if !pos-1 <> hi then
		match refseq with
			| None -> failwith "Alignment.MAF.stitch: discontiguity"
			| Some refseq ->
				if String.length refseq <> hi-lo+1 then
					failwith "Alignment.MAF.stitch: inappropriate reference sequence length"
				let piece = String.sub refseq (!pos - lo) (hi - !pos + 1)
				to_splice := ([|src_transform src|],[|piece|]) :: !to_splice
	
	if !refseq_mismatches * 10 > hi - lo + 1 then
		failwith "Alignment.MAF.stitch: reference sequence mismatch"
	
	Block.stitch ?missing_char ?order (List.rev !to_splice)

module DB = struct
	open Sqlite3EZ
	
	let blob_of_block { unparsed } =
		let input = IO.input_string unparsed
		let sofar = Buffer.create 256
		Zlib.compress
			fun buf -> try IO.input input buf 0 (String.length buf) with IO.No_more_input -> 0
			fun buf n -> Buffer.add_string sofar (String.sub buf 0 n)
		Buffer.contents sofar
			
	let block_of_blob blob =
		let input = IO.input_string blob
		let sofar = Buffer.create 256
		Zlib.uncompress
			fun buf -> try IO.input input buf 0 (String.length buf) with IO.No_more_input -> 0
			fun buf n -> Buffer.add_string sofar (String.sub buf 0 n)
		let blocks = parse (IO.input_string (Buffer.contents sofar))
		if Enum.count blocks <> 1 then failwith "Genome.Alignment.MAF.DB: corrupted block blob"
		Option.get (Enum.get blocks)
	
	type t = {
		h : Sqlite3EZ.db;
		stmt_query : statement;
		stmt_insert : statement;
	}
	
	let db_open ?(lockless=false) fn =
		let mode, vfs =
			if lockless then
				(Some `READONLY), (Some "unix-none")
			else
				None, None
		let h = db_open ?mode ?vfs fn

		exec h "CREATE TABLE IF NOT EXISTS MAFDB (ROWID INTEGER PRIMARY KEY,src TEXT NOT NULL,src_start_0inc INTEGER NOT NULL,src_end_0inc INTEGER NOT NULL,block BLOB NOT NULL)"
		
		{ h = h;
		  stmt_query = make_statement h "SELECT block FROM MAFDB WHERE src=? AND ROWID in (SELECT TargetROWID FROM MAFDB_RTREE WHERE src_end_0inc>=? AND src_start_0inc<=?)";
		  stmt_insert = make_statement h "INSERT INTO MAFDB VALUES(?,?,?,?,?)";
		}
		
	let db_close db = Sqlite3EZ.db_close db.h
	
	let with_db ?lockless fn f =
		let db = db_open ?lockless fn
		try
			let y = f db
			db_close db
			y
		with
			| exn ->
				db_close db
				raise exn
	
	let transact db f = Sqlite3EZ.transact db.h (fun _ -> f db)
	
	let query db ~src ~lo ~hi =
		if hi < lo then invalid_arg "Alignment.MAF.DB.query: hi < lo"
		let cons = function
			| [| Data.BLOB x |] -> block_of_blob x
			| _ -> failwith "Alignment.MAF.DB.query: unexpected result from SQLite"
		List.rev
			statement_query db.stmt_query
				[| Data.TEXT src;
				   Data.INT (Int64.of_int lo);
				   Data.INT (Int64.of_int hi) |]
				cons
				fun x lst -> x :: lst
				[]
				
	let sources db =
		let cons = function
			| [| Data.TEXT x|] -> x
			| _ -> failwith "Alignment.MAF.DB.sources: unexpected result from SQLite"
		List.rev
			statement_query (make_statement db.h "SELECT DISTINCT src FROM MAFDB")
				[||]
				cons
				fun x lst -> x :: lst
				[]
					
	let insert db block =
		let seq0 = block.sequences.(0)
		let blob = blob_of_block block
		statement_exec db.stmt_insert
			[| Data.NULL; Data.TEXT seq0.src; Data.INT (Int64.of_int seq0.start);
			   Data.INT (Int64.of_int (seq0.start + seq0.size - 1)); Data.BLOB blob |]
	
	let analyze db =
		exec db.h "CREATE INDEX IF NOT EXISTS MAFDB_SRC ON MAFDB(src); \
		           DROP TABLE IF EXISTS MAFDB_RTREE; \
				   CREATE VIRTUAL TABLE MAFDB_RTREE USING rtree_i32(TargetROWID,src_start_0inc,src_end_0inc); \
				   INSERT INTO MAFDB_RTREE SELECT ROWID, src_start_0inc, src_end_0inc FROM MAFDB; \
				   ANALYZE;"
