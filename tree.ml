type comparison = L | E | G;;

module type COMPARABLE_NODE=
	sig
		type t
		val compare : t -> t -> comparison
	end;;

module Set =
	functor(C : COMPARABLE_NODE) ->
	struct
			type element = C.t
			type node = Empty | Node of element*node*node			 
			let rec insert n t =
				match t with
					Empty -> Node(n, Empty, Empty)
					| Node(v, left, right) -> match C.compare n v with
																	E -> Node(v, left, right)
																	| L -> Node(v, insert n left, right)
																	| G -> Node(v, left, insert n right)
			let rec is_in n t =
				match t with
					Empty -> false
					| Node(v,left,right) ->match C.compare n v with
																| L -> is_in n left
																| G -> is_in n right
																| E -> true
	end;;

module Point =
	struct
		type t = {x: int; y:int}
		let compare a b =if a.x*a.y > b.x*b.y then G else if a.x*a.y<b.x*b.y then L else E
	end;;
