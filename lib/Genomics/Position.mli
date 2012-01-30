(** genomic sequence intervals in the style of the UCSC Genome Browser

These are in the form "seq:lo-hi" where lo and hi are one-based, inclusive integer coordinates. The
decimal string representation of the coordinates may contain commas. The genome assembly from which
the sequence is derived may also be specified, corresponding to the format e.g.
"hg19.chr22:1,234-4,321" *)

type t = string*string*int*int

val asmbl : t -> string
val seq : t -> string
val asmbl_seq : ?require:bool -> t -> string
val lo : t -> int
val hi : t -> int

val length : t -> int

val is : string -> bool
val of_string : string -> t
val split_asmbl_seq : string -> string*string

val to_string : ?asmbl:bool -> ?commas:bool -> t -> string
