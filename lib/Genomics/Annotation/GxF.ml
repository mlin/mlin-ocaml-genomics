open List
let (|>) x f = f x
exception Finalizer of exn
let finally g f x =
	try
		let y = f x
		try g () with exn -> raise (Finalizer exn)
		y
	with
		| Finalizer exn -> raise exn
		| exn ->
			try g () with _ -> ()
			raise exn

type feature = {
	seqid : string;
	lo : int;
	hi : int;
	source : string;
	ty : string;
	score : float option;
	strand : [`plus | `minus | `none | `unknown];
	phase : int option;
	id : string;
	attributes : (string*string) list;
	unparsed : string option
}

let choose_id_default lst = (try List.assoc "ID" lst with Not_found -> List.assoc "transcript_id" lst)

let re_feature = Str.regexp "^\
\\([^\t]+\\)\t\
\\([^\t]+\\)\t\
\\([^\t]+\\)\t\
\\([0-9]+\\)\t\
\\([0-9]+\\)\t\
\\([Ee0-9\\.\\+-]+\\)\t\
\\([-\\+\\?\\.]\\)\t\
\\(0\\|1\\|2\\|3\\|\\.\\)\t\
\\(.+\\)$"
let parse_feature ?(choose_id=choose_id_default) line =
	try
		if not (Str.string_match re_feature line 0) then raise Parsing.Parse_error
		let id, atts = match GxFAttParser.parse GxFAttLexer.token (Lexing.from_string (Str.matched_group 9 line)) with
			| `id id -> id, []
			| `keys_values lst -> (choose_id lst), lst
		{ seqid = Str.matched_group 1 line;
			source = Str.matched_group 2 line;
			ty = Str.matched_group 3 line;
			lo = int_of_string (Str.matched_group 4 line);
			hi = int_of_string (Str.matched_group 5 line);
			score = (match Str.matched_group 6 line with "." -> None | s -> Some (float_of_string s));
			strand = (match Str.matched_group 7 line with "+" -> `plus | "-" -> `minus | "." -> `none | "?" -> `unknown | _ -> raise Parsing.Parse_error);
			phase = (match Str.matched_group 8 line with "." -> None | "0" -> Some 0 | "1" -> Some 1 | "2" -> Some 2 | _ -> raise Parsing.Parse_error);
			id = id; attributes = atts; unparsed = Some line }
	with
		| exn -> raise exn (*failwith (Printf.sprintf "Annotation.GxF.parse_feature: %s\n%s" (Printexc.to_string exn) line)*)

let format_score = function
	| None -> "."
	| Some x when x = floor x -> string_of_int (int_of_float x)
	| Some x -> string_of_float x

let format_attributes_idonly ft = ft.id
let format_attributes_gtf ft = String.concat "; " (List.map (fun (k,v) -> Printf.sprintf "%s \"%s\"" k v) ft.attributes)
let format_attributes_gff ft = String.concat ";" (List.map (fun (k,v) -> k ^ "=" ^ v) ft.attributes)
let format_attributes_default ft = if ft.attributes = [] then format_attributes_idonly ft else format_attributes_gff ft
let feature_to_string ?(format_attributes=format_attributes_default) ft =
	match ft.unparsed with
		| Some line -> line
		| None ->
			Printf.sprintf "%s\t%s\t%s\t%d\t%d\t%s\t%c\t%c\t%s"
				ft.seqid
				ft.source
				ft.ty
				ft.lo
				ft.hi
				format_score ft.score
				match ft.strand with `plus -> '+' | `minus -> '-' | `none -> '.' | `unknown -> '?'
				match ft.phase with None -> '.' | Some 0 -> '0' | Some 1 -> '1' | Some 2 -> '2' | _ -> invalid_arg "GxF.feature_to_string: invalid phase"
				format_attributes ft
		
module DB = struct
	open Sqlite3EZ
		
	type db = {
		db : Sqlite3EZ.db;
		
		stmt_insert : statement;
		stmt_insert_alias : statement;
		stmt_query_id : statement;
		stmt_query_alias : statement;
		stmt_query_locus : statement;
		stmt_seqid_ids : statement;
	}

	let db_open ?(lockless=false) fn =
		let mode, vfs =
			if lockless then
				(Some `READONLY), (Some "unix-none")
			else
				None, None					
		let db = Sqlite3EZ.db_open ?mode ?vfs fn
		transact db
			fun _ ->
				exec db "CREATE TABLE IF NOT EXISTS GxF (ROWID INTEGER PRIMARY KEY,seqid TEXT NOT NULL,lo INTEGER NOT NULL CHECK (lo > 0),hi INTEGER NOT NULL,id TEXT NOT NULL,unparsed TEXT NOT NULL,CHECK (lo<=hi));\
					     CREATE TABLE IF NOT EXISTS GxF_ALIASES (targetROWID INTEGER NOT NULL REFERENCES GxF(ROWID),alias TEXT NOT NULL);"
		{
			db = db;
			stmt_insert = make_statement db "INSERT INTO GxF VALUES(?,?,?,?,?,?)";
			stmt_insert_alias = make_statement db "INSERT INTO GxF_ALIASES VALUES(?,?)";
			stmt_query_id = make_statement db "SELECT id,unparsed,seqid,lo,hi FROM GxF WHERE id=? ORDER BY seqid,lo,hi LIMIT ?";
			stmt_query_alias = make_statement db "SELECT id,unparsed,seqid,lo,hi FROM GxF WHERE ROWID IN (SELECT DISTINCT targetROWID FROM GxF_ALIASES WHERE alias=?) ORDER BY seqid,lo,hi LIMIT ?";
			stmt_query_locus = make_statement db "SELECT id,unparsed,lo,hi FROM GxF WHERE seqid=? AND ROWID in (SELECT ROWID FROM GxF_RTREE WHERE hi>=? AND lo<=?) ORDER BY lo,hi LIMIT ?";
			stmt_seqid_ids = make_statement db "SELECT DISTINCT id FROM GxF WHERE seqid=? ORDER BY id"
		}

	
	let db_close { db = db } = Sqlite3EZ.db_close db
	
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
			
	let transact d f = Sqlite3EZ.transact d.db (fun _ -> f d)	
	
	let analyze x =
		exec x.db "CREATE INDEX IF NOT EXISTS GxF_seqid ON GxF(seqid); \
			       CREATE INDEX IF NOT EXISTS GxF_id ON GxF(id); \
			       CREATE INDEX IF NOT EXISTS GxF_ALIASES_idx ON GxF_ALIASES(alias); \
			       DROP TABLE IF EXISTS GxF_RTREE; \
			       CREATE VIRTUAL TABLE GxF_RTREE USING rtree_i32(ROWID,lo,hi); \
			       INSERT INTO GxF_RTREE SELECT ROWID, lo, hi FROM GxF; \
			       ANALYZE;"

	let insert ?(aliases=[]) x ft =
		let unparsed = match ft.unparsed with Some txt -> txt | None -> feature_to_string ft
		statement_exec x.stmt_insert [| Data.NULL; Data.TEXT ft.seqid;
										Data.INT (Int64.of_int ft.lo); Data.INT (Int64.of_int ft.hi);
										Data.TEXT ft.id; Data.TEXT unparsed |]
		if aliases <> [] then
			let target_rowid = last_insert_rowid x.db
			aliases |> List.iter (fun alias -> statement_exec x.stmt_insert_alias
			                                       [| Data.INT target_rowid; Data.TEXT alias |])
	
	let cons_feature ar =
		if Array.length ar < 2 then failwith "GxF.DB: unexpected result from SQLite"
		let id = match ar.(0) with
			| Data.TEXT text -> text
			| _ -> failwith "GxF.DB: unexpected result from SQL query"
		let unparsed = match ar.(1) with
			| Data.TEXT text -> text
			| _ -> failwith "GxF.DB: unexpected result from SQL query"
		parse_feature ~choose_id:(fun _ -> id) unparsed
	
	let fold ?(sort=false) f x init =
		let sql = 
			if sort then
				"SELECT id,unparsed,seqid,lo,hi FROM GxF ORDER BY seqid,lo,hi"
			else
				"SELECT id,unparsed FROM GxF"
		statement_query (make_statement x.db sql) [||] cons_feature f init

	let ids x =
		List.rev
			statement_query (make_statement x.db "SELECT DISTINCT id FROM GxF ORDER BY id") [||]
				function [| Data.TEXT id |] -> id | _ -> assert false
				fun id lst -> id :: lst
				[]

	let seqids x =
		List.rev
			statement_query (make_statement x.db "SELECT DISTINCT seqid FROM GxF ORDER BY seqid") [||]
				function [| Data.TEXT seqid |] -> seqid | _ -> assert false
				fun seqid lst -> seqid :: lst
				[]
				
	let seqid_ids x seqid =
		List.rev
			statement_query x.stmt_seqid_ids
				[| Data.TEXT seqid |]
				function [| Data.TEXT id |] -> id | _ -> assert false
				fun id lst -> id :: lst
				[]
			 
	let statement_query_features stmt p = List.rev (statement_query stmt p cons_feature (fun ft sofar -> ft :: sofar) [])
		
	let query_id ?(limit=(-1)) x id = statement_query_features x.stmt_query_id [| Data.TEXT id; Data.INT (Int64.of_int limit) |]
		
	let query_alias ?(limit=(-1)) x alias = statement_query_features x.stmt_query_alias [| Data.TEXT alias; Data.INT (Int64.of_int limit) |]

	let query_seqid ?(limit=(-1)) x seqid =
		statement_query_features
			make_statement x.db "SELECT id,unparsed,lo,hi FROM GxF WHERE seqid=? ORDER BY lo,hi LIMIT ?"
			[| Data.TEXT seqid; Data.INT (Int64.of_int limit) |]
		
	let query_locus ?(limit=(-1)) x seqid lo hi =
		if hi < lo then invalid_arg "Annotation.GxF.DB.query_locus: hi < lo"
		statement_query_features x.stmt_query_locus
			[| Data.TEXT seqid; Data.INT (Int64.of_int lo); Data.INT (Int64.of_int hi); Data.INT (Int64.of_int limit) |]
