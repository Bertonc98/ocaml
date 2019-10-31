module type StackADT =
	sig
		type 'a stack = { mutable c : 'a list; }
		exception Empty_stack
		val push : 'a stack -> 'a -> unit
		val top : 'a list -> 'a
		val pop : 'a stack -> unit
		val empty : unit -> 'a stack
	end;;


module Polish(Stack:StackADT)=
  struct
    type expr = Val of int
                |Sum of expr*expr
                |Diff of expr*expr
                |Mul of expr*expr
                |Div of expr*expr
  end;;
