

(* Serve un abstract data type, la signature delle operazioni necessarie *)
module type OpVarADT =
  sig
    type a and b and c
    val op: a->b->c
    val init : c
  end;;
(* Serve anche un funtore, che utilizza la signature OpVarADT per assicurare quelle operazioni *)
module VarArgs (OP : OpVarADT) =
  struct
    let arg x = fun y rest -> rest (OP.op x y)
    let stop x = x
    let f g = g OP.init
  end;;


(*let arg x = fun y rest -> rest (op x y);;
let stop x = x;;
let f g = g init;; *)
