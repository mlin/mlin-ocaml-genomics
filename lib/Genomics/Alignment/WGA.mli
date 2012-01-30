(** ties together a few things in the library to make it easier to work with MULTIZ/TBA 
whole-genome multiple alignments (e.g. those from the UCSC Genome Browser)

Concept: MAF whole-genome alignments do not necessarily cover the entire reference sequence. Hence,
to produce a convenient [Block.t] alignment of an arbitrary genomic region, we need access to both
the MAF alignment blocks and the primary sequence of the reference genome. Thus, we build one
database that contains both [MAF.DB] and [Sequence.DB] tables, and provide routines in this module
to load the primary sequence, then the pertinent MAF blocks, and put it all together.

There is also a facility for automatically mapping browser assembly names (e.g. "hg19") to friendly
names (e.g. "Human") in the returned alignment blocks. *)

type db

val db_open : ?assembly_map:string -> ?lockless:bool -> string -> db
val db_close : db -> unit
val with_db : ?assembly_map:string -> ?lockless:bool -> string -> (db -> 'a) -> 'a

val assembly_order : db -> string array option

val query_position : ?missing_char:char -> db -> Position.t -> (string array)*Block.t

