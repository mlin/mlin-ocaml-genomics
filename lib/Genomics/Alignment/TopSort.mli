
module type S = sig
	type t
	module Items : Set.S with type elt = t
	module Constraints : Set.S with type elt = t*t
	val partial_order : t list -> Constraints.t
	val sort : Items.t -> Constraints.t -> t list
	exception Unsat of Items.t
	
module Make : functor (T : Set.OrderedType) -> S with type t = T.t

