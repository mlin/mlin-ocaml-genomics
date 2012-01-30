open List

module type S = sig
	type t
	module Items : Set.S with type elt = t
	module Constraints : Set.S with type elt = (t*t)
	val partial_order : t list -> Constraints.t
	val sort : Items.t -> Constraints.t -> t list
	exception Unsat of Items.t
	
module Make = functor (T : Set.OrderedType) -> struct
	type t = T.t
	
	module Constraints = Set.Make(struct type t = T.t*T.t let compare = compare end)

	let partial_order lst =
		let rec f = function
			| fst :: ((snd :: _) as rest) -> Constraints.add (fst,snd) (f rest)
			| _ -> Constraints.empty
		f lst
		
	module Items = Set.Make(struct type t = T.t let compare = compare end)
	
	exception Unsat of Items.t
	
	let rec step items constraints =
		let (constrained,unconstrained) = Items.partition (fun x -> Constraints.exists (fun (_,y) -> x = y) constraints) items
		if unconstrained = Items.empty then
			if constraints <> Constraints.empty then
				(* uh oh...constraint graph has cycles *)
				(* first see if there are any (a,b) (b,a) cycles *)
				let simply_bad = ref Items.empty
				Constraints.iter (fun (a,b) -> if Constraints.mem (b,a) constraints then simply_bad := Items.add a (Items.add b !simply_bad)) constraints
				if !simply_bad <> Items.empty then
					raise (Unsat !simply_bad)
				else
					(* otherwise just spit out everything remaining *)
					let bad = Constraints.fold (fun (fst,snd) set -> Items.add fst (Items.add snd set)) constraints Items.empty
					raise (Unsat bad)
			[]
		else
			let unsatisfied = Constraints.filter (fun (x,_) -> not (Items.mem x unconstrained)) constraints
			(Items.elements unconstrained) :: (step constrained unsatisfied)
	
	let sort items constraints =
		let items' = Constraints.fold (fun (x,y) set -> Items.add x (Items.add y set)) constraints Items.empty
		if Items.cardinal (Items.diff items' items) > 0 then failwith "TopSort: constraints on unknown items"
		flatten (step items constraints)
