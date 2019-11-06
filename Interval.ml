open IntervalI;;
open Comparable;;

module Interval(Endpoint:Comparable)=
	struct
		type endpoint = Endpoint.t;;
		type interval = Interval of endpoint*endpoint | Empty;;
		exception WrongInterval;;
		
		let create low high =
			if Endpoint.compare low high > 0 then raise WrongInterval
			else if Endpoint.compare low high = 0 then Empty
			else Interval(low,high);;
		
		let is_empty intervallo =
			match intervallo with
				Empty->true
				|Interval(_,_)-> false;;
		
		let contains intervallo elm =
			match intervallo with
				Empty->false
				|Interval(a,b)->(Endpoint.compare a elm <= 0) && (Endpoint.compare elm b <= 0);;
		
		let min a b =
			if (Endpoint.compare a b) <=0 then a
			else b;;
			
		let max a b =
			if (Endpoint.compare a b) <=0 then b
			else a;;
		
		let intersect int1 int2 =
			match int1,int2 with
				Empty,_|_,Empty->Empty
				|Interval(a1,b1),Interval(a2,b2)-> Interval((max a1 a2), (min b1 b2));;
		
		let tostring intervallo =
			match intervallo with
				Empty -> "Empty"
				|Interval(a,b)-> "[" ^ (Endpoint.tostring a) ^ "," ^ (Endpoint.tostring b) ^ "]";;
	end;;

module IntInterval = Interval(struct 
								type t = int
								let compare a b = a-b
								and tostring a = string_of_int a
							  end);;
							  
module StringInterval = Interval(struct 
									type t = string
									let compare a b = String.compare a b
									and tostring a = a
								  end);;								
