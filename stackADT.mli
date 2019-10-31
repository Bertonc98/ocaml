module type StackADT =
	sig
		type 'a stack = { mutable c : 'a list; }
		exception EmptyStack
		val push : 'a stack -> 'a -> unit
		val top : 'a stack -> 'a
		val pop : 'a stack -> 'a
		val empty : unit -> 'a stack
	end;;
