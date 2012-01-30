open Batteries_uni
open Sqlite3EZ
	
let compress txt =
	let input = IO.input_string txt
	let sofar = Buffer.create 256
	Zlib.compress
		fun buf -> try IO.input input buf 0 (String.length buf) with IO.No_more_input -> 0
		fun buf n -> Buffer.add_string sofar (String.sub buf 0 n)
	Buffer.contents sofar

let uncompress blob =
	let input = IO.input_string blob
	let sofar = Buffer.create 256
	Zlib.uncompress
		fun buf -> try IO.input input buf 0 (String.length buf) with IO.No_more_input -> 0
		fun buf n -> Buffer.add_string sofar (String.sub buf 0 n)
	Buffer.contents sofar
	
let assemble pieces lo hi =
	if pieces = [] then failwith "Sequence.DB.query: no coverage of desired region"
	let buf = Buffer.create 256
	let rec loop pos = function
		| (plo,phi,pseq) :: rest when phi < lo -> loop pos rest
		| (plo,phi,pseq) :: rest when plo > hi -> loop pos rest
		| (plo,phi,pseq) :: rest when pos >= plo ->
			assert (String.length pseq = phi-plo+1)
			if pos > plo && Buffer.length buf > 0 then failwith "Sequence.DB.query: overlapping entries for desired region"
			let dlo = max pos plo
			let dhi = min hi phi
			assert (dhi >= dlo)
			Buffer.add_string buf (String.sub pseq (dlo - plo) (dhi - dlo + 1))
			loop (dhi+1) rest
		| [] when pos = hi+1 ->
			assert (Buffer.length buf = hi-lo+1)
			Buffer.contents buf
		| _ -> failwith "Sequence.DB.query: discontiguous coverage of desired region"
	loop lo (List.sort pieces)

type t = {
	h : Sqlite3EZ.db;
	stmt_query : statement;
	stmt_range : statement;
	stmt_insert : statement;
	
	mutable last_query : (string*int*int*((int*int*string) list)) option;
}

let db_open ?(lockless=false) fn =
	let mode, vfs =
		if lockless then
			(Some `READONLY), (Some "unix-none")
		else
			None, None
	let h = db_open ?mode ?vfs fn
			
	exec h "CREATE TABLE IF NOT EXISTS SequenceDB (ROWID INTEGER PRIMARY KEY,seqid TEXT NOT NULL,lo_1inc INTEGER NOT NULL, hi_1inc INTEGER NOT NULL,compressed BLOB NOT NULL)"
		
	{ h = h;
		stmt_query = make_statement h "SELECT lo_1inc,hi_1inc,compressed FROM SequenceDB where seqid=? AND ROWID IN (SELECT TargetROWID FROM SequenceDB_RTREE WHERE hi_1inc>=? AND lo_1inc<=?)";
		stmt_range = make_statement h "SELECT MIN(lo_1inc), MAX(hi_1inc) FROM SequenceDB WHERE seqid=?";
		stmt_insert = make_statement h "INSERT INTO SequenceDB VALUES(?,?,?,?,?)";
		
		last_query = None
	}
		
let db_close db =
	db.last_query <- None
	db_close db.h

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

let query db ~id ~lo ~hi =
	if hi < lo then invalid_arg "Sequence.DB.query_locus: hi < lo"
	match db.last_query with
		| Some (id',lo',hi',pieces) when id = id' && lo' <= lo && hi' >= hi -> assemble pieces lo hi
		| _ ->
			let msg = "Sequence.DB.query: unexpected result from SQLite"
			let cons ar =
				if Array.length ar <> 3 then failwith msg
				let plo = match ar.(0) with Data.INT x -> Int64.to_int x | _ -> failwith msg
				let phi = match ar.(1) with Data.INT x -> Int64.to_int x | _ -> failwith msg
				let pseq = match ar.(2) with Data.BLOB x -> uncompress x | _ -> failwith msg
				(plo,phi,pseq)
			let pieces =
				List.sort
					statement_query
						db.stmt_query
						[| Data.TEXT id; Data.INT (Int64.of_int lo); Data.INT (Int64.of_int hi) |]
						cons
						(fun x lst -> x :: lst)
						[]
			if pieces <> [] then
				let all_lo = List.fold_left (fun sofar (lo,_,_) -> min lo sofar) max_int pieces
				let all_hi = List.fold_left (fun sofar (_,hi,_) -> max hi sofar) min_int pieces
				db.last_query <- Some (String.copy id, all_lo, all_hi, pieces)
			assemble pieces lo hi

let ids db =
	let msg = "Sequence.DB.ids: unexpected result from SQLite"
	let cons ar =
		if Array.length ar <> 1 then failwith msg
		match ar.(0) with Data.TEXT x -> x | _ -> failwith msg
	statement_query
		make_statement db.h "SELECT DISTINCT seqid FROM SequenceDB"
		[||]
		cons
		(fun x lst -> x :: lst)
		[]
			
let range db ~id =
	let msg = "Sequence.DB.range: unexpected result from SQLite"
	let cons ar = match ar with
		| [| Data.INT lo; Data.INT hi |] -> Some (Int64.to_int lo, Int64.to_int hi)
		| _ -> failwith msg
	Option.get
		statement_query
			db.stmt_range
			[| Data.TEXT id |]
			cons
			fun x -> function None -> x | Some _ -> failwith msg
			None

let insert db ~id ~lo ~subseq =
	statement_exec db.stmt_insert
		[| Data.NULL; Data.TEXT id; Data.INT (Int64.of_int lo);
			Data.INT (Int64.of_int (lo + String.length subseq - 1));
			Data.BLOB (compress subseq) |]

let analyze db =
	exec db.h "CREATE INDEX IF NOT EXISTS SequenceDB_seqid ON SequenceDB(seqid); \
				DROP TABLE IF EXISTS SequenceDB_RTREE; \
				CREATE VIRTUAL TABLE SequenceDB_RTREE USING rtree_i32(TargetROWID,lo_1inc,hi_1inc); \
				INSERT INTO SequenceDB_RTREE SELECT ROWID, lo_1inc, hi_1inc FROM SequenceDB; \
				ANALYZE;"

let transact db f = Sqlite3EZ.transact db.h (fun _ -> f db)
