type comparison = G | L | E;;

module type ORDERED_TYPE=
	sig
		type t
		val compare: t->t->comparison
	end;;

module Set=
	functor(E : ORDERED_TYPE)->
	struct
		type element = E.t
		type set = element list
		let empty = []
		let rec add x s =
			match s with
				[] -> [x]
				| h :: t -> match E.compare x h with
									E -> s
									| L -> x::s
									| G -> h::(add x t)
		let rec is_in x s=
			match s with
				[]->false
				|h::t-> match E.compare x h with
									E  -> true
									|L -> false
									|G -> is_in x t
	end;;
	
module OrderedString =
	struct
		type t = string
		let compare x y = if x=y then E 
											else if x<y then L 
											else G
	end;;
	
module StringSet = Set(OrderedString : ORDERED_TYPE);;
