open Batteries
open Sqlite3EZ

type db = {
	seqdb : Sequence.DB.t;
	mafdb : MAF.DB.t;
	default_ref_assembly : string option;
	assembly_map : (string,string) Map.t option;
	assembly_order : string array option
}

let db_open ?(assembly_map="default") ?(lockless=false) fn =
	if not (Sys.file_exists fn) then failwith "Alignment.WGA.db_open: database not found"
	let default_ref_assembly, assembly_map, assembly_order = 
		try
			if assembly_map = "" then raise Not_found
			let mode, vfs =
				if lockless then
					(Some `READONLY), (Some "unix-none")
				else
					None, None
			Sqlite3EZ.with_db ?mode ?vfs fn
				fun db ->
					let data =
						List.rev
							statement_query
								make_statement db "SELECT assembly, name FROM AssemblyMap WHERE map=? ORDER BY ord"
								[|Data.TEXT assembly_map|]
								function
									| [| Data.TEXT assembly; Data.TEXT name |] -> assembly, name
									| [| Data.TEXT assembly; Data.NULL |] -> assembly, assembly
									| _ -> failwith "Alignment.WGA.db_open: unexpected result while loading assembly map"
								fun p lst -> p :: lst
								[]

					if data = [] then raise Not_found

					let assembly_order =
						Array.of_list
							List.rev
								List.fold_left
									fun uniq (_,name) ->
										let differs = uniq = [] || List.hd uniq <> name
										if differs then name :: uniq else uniq
									[]
									data
								
					let assembly_map =
						List.fold_left
							fun map (assembly,name) -> Map.add assembly name map
							Map.empty
							data
							
					let default_ref_assembly = fst (List.hd data)
							
					(Some default_ref_assembly), (Some assembly_map), (Some assembly_order)
		with _ -> None, None, None
	
	let seqdb = Sequence.DB.db_open ~lockless fn
	let mafdb = MAF.DB.db_open ~lockless fn
	{ seqdb = seqdb; mafdb = mafdb; default_ref_assembly = default_ref_assembly;
	  assembly_map = assembly_map; assembly_order = assembly_order }
	
let db_close { seqdb; mafdb }=
	Sequence.DB.db_close seqdb
	MAF.DB.db_close mafdb

let with_db ?assembly_map ?lockless fn f =
	let db = db_open ?assembly_map ?lockless fn
	try
		let y = f db
		db_close db
		y
	with
		| exn ->
			db_close db
			raise exn

let assembly_order { assembly_order = maybe } = Option.map Array.copy maybe

let assembly_name src = try String.left src (String.index src '.') with	Not_found -> src
	
let src_transform = function
	| None -> assembly_name
	| Some assembly_map ->
		fun src ->
			let asmbl = assembly_name src
			try
				Map.find asmbl assembly_map
			with Not_found -> asmbl (*failwith (Printf.sprintf "Alignment.WGA.query_position: could not find %s in the assembly name map" src)*)

let query_position ?missing_char { seqdb; mafdb; default_ref_assembly; assembly_map; assembly_order } pos = 
	let pos = match pos, default_ref_assembly with
		| ("",seq,lo,hi), Some asmbl -> (asmbl,seq,lo,hi)
		| _ -> pos
	if Position.hi pos < Position.lo pos then invalid_arg "Alignment.WGA.query_position: hi < lo"
	let refseq = Sequence.DB.query seqdb (Position.asmbl_seq pos) (Position.lo pos) (Position.hi pos)
	assert (String.length refseq = Position.length pos)
	let mafs = MAF.DB.query mafdb (Position.asmbl_seq pos) (Position.lo pos - 1) (Position.hi pos - 1)
	MAF.stitch
		~src_transform:(src_transform assembly_map)
		?missing_char
		~refseq
		?order:assembly_order
		~src:(Position.asmbl_seq pos)
		~lo:(Position.lo pos - 1)
		~hi:(Position.hi pos - 1)
		mafs
