module type Signature =
  sig
    type a and b and c
    val op : a->b->c
    val init: c
  end;;

module AbsVarargs (OP:Signature) =
  struct
    let stop x = x
    let arg x = fun y rest -> rest (OP.op x y)
    let f g = g(OP.init)
  end;;

(* Example of use *)
module Sum =
  struct
    type a=int and b=int and c=int
    let op = fun x y -> x+y
    let init = 0
  end;;
module StringConcat =
  struct
    type a=string and b=string list and c=string list
    let op = fun (x: string) y -> y @ [x]
    let init = []
  end;;
module IntConcat (T:sig type t end) =
  struct
    type a=T.t and b=a list and c = a list
    let op = fun (x:T.t) y -> y @ [x]
    let init = []
  end;;

(* create a varargs to sum int *)
module M0 = AbsVarargs(Sum);;

(* create a varargs to concat strings into a list *)
module M1 = AbsVarargs(StringConcat);;

(* create a varargs to concat int *)
module M3 = AbsVarargs(IntConcat(struct type t=int end));;
