module Pstack=
  struct
    type stack = { mutable c: string list}

    exception Empty_Stack

    let empty()={c=[]}

    let push s el = s.c<-el::s.c

    let pop s =
      match s.c with
        []->raise Empty_Stack
        |h::t->s.c<-t

    let top s =
      match s.c with
        []->raise Empty_Stack
        |h::t->h
  end;;
