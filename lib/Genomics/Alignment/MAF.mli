(** Multiple Alignment Format (MAF)
@see <http://genome.ucsc.edu/FAQ/FAQformat#format5> MAF format (UCSC)
*)

open Batteries_uni

type sequence = {
	src : string;
	start : int;	(** ZERO-based, inclusive coordinate *)
	size : int;
	strand : [`plus | `minus];
	src_size : int;
	text : string;
}

type block = {
	attributes : (string*string) list;
	sequences : sequence array;
	
	unparsed : string; (** original text of this MAF block *)
}

val block : block -> Block.t

(** retrieve the [src] field of each sequence *)
val block_sources : block -> string array

val parse : IO.input -> block Enum.t

(**/**)
val parse_minimally : IO.input -> block Enum.t
(**/**)

(** storage of MAF blocks in a database, enabling efficient retrieval by genomic interval *)
module DB : sig
	type t
	
	(** as [Sqlite3.db_open] *)
	val db_open : ?lockless:bool -> string -> t
	
	(** close the database immediately; otherwise the GC will take care of it *)
	val db_close : t -> unit
	
	val with_db : ?lockless:bool -> string -> (t -> 'a) -> 'a

	(** [query db src lo hi] retrieves all MAF blocks overlapping the interval [\[lo,hi\]] in the
	source sequence [src]. Here [lo] and [hi] are zero-based, inclusive coordinates. All blocks
	overlapping any part of the requested interval are returned, so individual blocks may extend
	beyond [lo] and/or [hi]. *)
	val query : t -> src:string -> lo:int -> hi:int -> block list

	(** enumerate all source sequences that are the reference (first) sequence of at least one MAF
	    block in the database *)
	val sources : t -> string list

	(** insert a new block into the database, No duplication or overlap detection is performed. *)	
	val insert : t -> block -> unit
	
	(** build and analyze database indices. should be done once after all insertions are completed.
		*)
	val analyze : t -> unit
	
	(** for performance reasons, insertions should be performed within a transaction *)
	val transact : t -> (t -> 'a) -> 'a

(** [stitch blocks src lo hi] stitches a contiguous alignment of the interval [\[lo,hi\]] on a
reference sequence [src] given the pertinent MAF blocks. Here [lo] and [hi] are zero-based,
inclusive coordinates. The first sequence of each block in [blocks] must match [src]. The blocks
must be non-overlapping.

@return an alignment block for exactly the interval [\[lo,hi\]] of the reference sequence, with the
name (src) of each row.
@param order as in {!Block.stitch}
*)
val stitch : ?src_transform:(string->string) -> ?missing_char:char -> ?refseq:string -> ?order:(string array) -> src:string -> lo:int -> hi:int -> block list -> (string array)*Block.t
