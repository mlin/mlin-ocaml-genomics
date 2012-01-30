(** generic operations for one-dimensional intervals *)

module type Interval = sig
	type t (** an interval [\[lo,hi\]] (inclusive) *)
	val lo : t -> int
	val hi : t -> int
(** Your interval type *)

module type S = sig
	type t
	
	(** intervals are sorted by their low index first *)
	val compare : t -> t -> int

	val equal : t -> t -> bool

	val disjoint : t -> t -> bool

	val overlap : t -> t -> bool

	(** [contains a b] returns true iff [a] contains [b]. [contains a a = true] *)
	val contains : t -> t -> bool

	val sort : t list -> t list

	(** [enclose ivals] returns the smallest interval containing [ivals] *)
	val enclose : t list -> (int*int)
	
	(** [union ivals] returns the interval union of [ivals], sorted in increasing order. [ivals]
        must be sorted. *)
	val union : t list -> (int*int) list

	(** [intersect ivals] returns the intersection of [ivals], if any *)
	val intersect : t list -> (int*int) option

	(** [cluster ivals] partitions the input list into groups containing overlapping intervals (or
		intervals separated by no more than [distance]). [ivals] must be sorted. *)
	val cluster : ?distance:int -> t list -> t list list
(** Signature for interval operations over your data type *)

(** Functor building the interval utilities over your datatype. *)
module Make (Q : Interval) : S with type t = Q.t
