module type Interval = sig
	type t
	val lo : t -> int
	val hi : t -> int

module type S = sig
	type t
	
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val disjoint : t -> t -> bool
	val overlap : t -> t -> bool
	val contains : t -> t -> bool
	val sort : t list -> t list
	val enclose : t list -> int*int
	val union : t list -> (int*int) list
	val intersect : t list -> (int*int) option
	val cluster : ?distance:int -> t list -> t list list

module Make = functor (Q : Interval) -> struct
	type t = Q.t
	
	let compare a b =
		match compare (Q.lo a) (Q.lo b) with
			| 0 -> compare (Q.hi a) (Q.hi b)
			| x -> x

	let length ival = (Q.hi ival) - (Q.lo ival) + 1
	let equal a b = (compare a b) = 0
	let disjoint a b = (Q.hi a) < (Q.lo b) || (Q.hi b) < (Q.lo a)
	let overlap a b = not (disjoint a b)
	let contains a b = ((Q.lo a) <= (Q.lo b)) && ((Q.hi a) >= (Q.hi b))
	let sort = List.sort compare

	let enclose ivals =
		let rec iter minsofar maxsofar = function
			| ival :: rest -> iter (min (Q.lo ival) minsofar) (max (Q.hi ival) maxsofar) rest
			| [] -> (minsofar,maxsofar)
		match ivals with
			| fst :: rest -> iter (Q.lo fst) (Q.hi fst) rest
			| [] -> invalid_arg "Interval.enclose: empty input"

	let intersect ivals =
		let rec iter minsofar maxsofar = function
			| ival :: rest ->
				let newmin = max (Q.lo ival) minsofar
				let newmax = min (Q.hi  ival) maxsofar
				if newmin <= newmax then
					iter newmin newmax rest
				else
					None
			| [] -> Some (minsofar,maxsofar)
		match ivals with
			| fst :: rest -> iter (Q.lo fst) (Q.hi fst) rest
			| [] -> None

	let cluster ?(distance=0) ivals =
		let rec iter last_lo last_hi cluster clusters = function
			| ival :: rest when (Q.lo ival) < last_lo -> invalid_arg "Interval.cluster: input list must be sorted"
			| ival :: rest when (Q.lo ival) <= last_hi -> iter (Q.lo ival) (max last_hi ((Q.hi ival) + distance)) (ival :: cluster) clusters rest
			| ival :: rest -> iter (Q.lo ival) ((Q.hi ival) + distance) [ival] ((List.rev cluster) :: clusters) rest
			| [] -> List.rev ((List.rev cluster) :: clusters)
		match ivals with
			| fst :: rest -> iter (Q.lo fst) ((Q.hi fst) + distance) [fst] [] rest
			| [] -> []
			
	let union ivals = List.map enclose (cluster ivals)

	module DB = struct
		type db = {
			ival : t;
			mutable instances : t list;
			mutable max : int;
			mutable left : db option;
			mutable right : db option
		}

		let rec insert tree ival =
			let cmp = compare ival tree.ival
			if cmp < 0 then
				match tree.left with
					| Some l ->
						insert l ival
						tree.max <- max tree.max l.max
					| None ->
						tree.left <- Some { ival = ival; instances = [ival]; max = Q.hi ival; left = None; right = None }
						tree.max <- max tree.max (Q.hi ival)
			else if cmp > 0 then
				match tree.right with
					| Some r ->
						insert r ival
						tree.max <- max tree.max r.max
					| None ->
						tree.right <- Some { ival = ival; instances = [ival]; max = Q.hi ival; left = None; right = None }
						tree.max <- max tree.max (Q.hi ival)
			else
				tree.instances <- ival :: tree.instances	

		let create ivals =
			if ivals = [] then invalid_arg "Interval.DB.create: empty input"
			let tree = { ival = (List.hd ivals); instances = [List.hd ivals]; max = (Q.hi (List.hd ivals)); left = None; right = None }
			List.iter (insert tree) (List.tl ivals)
			tree

		(* postorder traversal *)
		let fold f init tree =
			let rec g rightmore = function
				| None -> rightmore
				| Some { instances = ivals; left = l; right = r } -> g (List.fold_right f ivals (g rightmore r)) l
			g init (Some tree)

		let intervals =  fold (fun ival lst -> ival :: lst) []

		let size = fold (fun _ n -> n+1) 0

		let height tree =
			let rec f = function
				| Some tree -> 1 + (max (f tree.left) (f tree.right))
				| None -> 0
			f (Some tree)

		let query tree lo hi =
			let desired ival = not (((Q.hi ival) < lo) || (hi < (Q.lo ival)))
			let rec f rightmore = function
				| None -> rightmore
				| Some {max = m} when m < lo -> rightmore
				| Some {ival = i; left = l} when Q.lo i > hi -> f rightmore l
				| Some {ival = i; instances = ivals; left = l; right = r} when desired i -> f (ivals @ (f rightmore r)) l
				| Some {ival = i; left = l; right = r} -> f (f rightmore r) l
			f [] (Some tree)
