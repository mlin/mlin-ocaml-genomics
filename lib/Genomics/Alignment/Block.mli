(** individual alignment blocks *)

open Batteries

(** an alignment block is an array of strings, each with the same length *)
type t = string array

(** reverse each sequence in the alignment (does NOT complement it!) *)
val reverse : t -> t

(** map each individual character *)
val map : (char->char) -> t -> t

(** reverse the alignment and map each individual character (e.g. complement) *)
val revmap : (char->char) -> t -> t

(** take a subsequence of the columns *)
val sub_cols : t -> start:int -> length:int -> t

(** construct an enumeration of the alignment columns *)
val cols : t -> (char array) Enum.t

(** remove any columns that are gapped in the specified row (discarding any other information in the
column)
@param row: which row to remove gaps from (default 0, the first row) *)

val rm_gaps : ?row:int -> t -> t

(** a reference sequence is an ungapped sequence *)
type refseq = string

(** compute the reference sequence from an alignment row by removing gaps
@return the reference sequence and maps for converting positions between alignment columns and the
reference sequence
@param row which row to use (zero-based). if unspecified, the first row is used
*)
val refseq : ?row:int -> t -> refseq*[`RefToCol of int array]*[`ColToRef of int array]

(** As an example, suppose we have a pairwise alignment [block] and we want to know what is aligned
to the [k]'th position of the reference (first) sequence.

{[let informant_character block k =
  let refseq, (`RefToCol ref2col), (`ColToRef col2ref) = Block.refseq block
  block.(1).[ref2col.(k)] ]}
*)

type names = string array

(** concatenate several alignment blocks when the blocks have different rows (species) present or
absent. The argument is a list of blocks with a name for each row. By default, the order of the
names from all the blocks must be consistent (i.e. the names must be topologically sortable)

@return a new block with a row for each distinct name given with the input blocks, with the
concatenated sequences of rows with matching names from each block. Positions of each concatenad
sequence corresponding to blocks not containing a row with the respective name are filled in using
[missing_char].
@param missing_char the character that will be used to fill in the positions corresponding to
species that are missing in the individual blocks. Defaults to - @param order if left unspecified,
the order of the rows in the concatenated block is determined by topologically sorting the input
names. This can be used to override this order. You can even add dummy rows that do not actually
appear in the blocks, which will be filled in entirely with [missing_char]. *)
val stitch : ?missing_char:char -> ?order:names -> (names*t) list -> names*t

val input_mfa : IO.input -> names*t
