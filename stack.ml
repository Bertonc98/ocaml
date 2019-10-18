module Stack =
  struct
    type 'a stack = { mutable c : 'a list; }

    exception Empty_stack

    let push s el = s.c<-el::s.c

    let top = function
      [] -> raise Empty_stack
      | h::_ -> h

    let pop s =
      match s.c with
        [] -> raise Empty_stack
        | h::t -> s.c <-t

    let empty() = {c=[]};;
    
    let not_user_usable_function() = print_string("Non dovresti vedere ciò\n")
  end;;

module type STACK =
	sig
		type 'a stack = { mutable c : 'a list; }
		exception Empty_stack
		val push : 'a stack -> 'a -> unit
		val top : 'a list -> 'a
		val pop : 'a stack -> unit
		val empty : unit -> 'a stack
	end;;
