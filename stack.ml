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
  end;;
