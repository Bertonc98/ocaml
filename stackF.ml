module type StackSig =
  sig
    type a
  end;;

module AbsStack (S:StackSig)=
  struct

    type stack ={mutable c : S.a list}

    exception Empty_stack

    let push s el = s.c<-el::s.c

    let pop s =
      match s.c with
        []->raise Empty_stack
        |h::t->s.c<-t

    let top s =
      match s.c with
        []-> raise Empty_stack
        |h::_->h
    let empty = {c=[]}
  end;;

module Intero =
  struct
    type a=int
  end;;
