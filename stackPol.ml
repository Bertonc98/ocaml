module StackPol =
  struct
    exception EmptyStack

    type 'a stack = {mutable c: 'a list};;

    let push s elm = s.c<-elm::s.c;;

    let empty() = {c=[]};;

    let top s =
      match s.c with
        h::t->h
        |[]->raise EmptyStack

    let pop s =
      match s.c with
        h::t->s.c<-t;h
        |[]->raise EmptyStack
  end;;
