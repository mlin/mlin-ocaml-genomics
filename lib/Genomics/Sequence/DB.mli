(** compressed storage of long sequences (e.g. chromosomal DNA assemblies) with efficient retrieval
of subsequences *)

type t

(** as [Sqlite3.db_open] *)
val db_open : ?lockless:bool -> string -> t

(** close the database immediately; otherwise the GC will take care of it. *)
val db_close : t -> unit

val with_db : ?lockless:bool -> string -> (t -> 'a) -> 'a

(** [query db id lo hi] retrieves the subsequence [id:lo-hi] where [lo] and [hi] are one-based,
inclusive coordinates. If the desired subsequence is stored across multiple records in the database,
these are 'assembled' as necessary to produce exactly the requested region.

@raise Failure if the subsequence cannot be retrieved unambiguously (not present, discontiguous or
multiple overlapping subsequences) *)
val query : t -> id:string -> lo:int -> hi:int -> string

(** enumerate the IDs of the sequences available from the database *)
val ids : t -> string list

(** determine the minimum and maximum coordinates stored for a sequence. Does not consider possible
discontiguities in that interval. Coordinates are one-based, inclusive. *) 
val range : t -> id:string -> int*int

(** insert a subsequence into the database.
	@param id the ID of the sequence you want to insert
	@param seq the subsequence you want to insert
	@param lo coordinate of the first position of the subsequence along the complete sequence; this
				is a one-based, inclusive coordinate
	
	Long sequences such as chromosomes should be 'shredded' and inserted into the database as many
	smaller subsequences. Then [query] will load just the pertinent subsequences in order to produce
	the sequence of any requested region. This function does not check to see if there is already an
	overlapping subsequence in the database. *)
val insert : t -> id:string -> lo:int -> subseq:string -> unit

(** build and analyze database indices. Should be called once after completing all inserts *)
val analyze : t -> unit

(** for performance reasons, multiple insertions should be carried out within a transaction *)
val transact : t -> (t -> 'a) -> 'a
