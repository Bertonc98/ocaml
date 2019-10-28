module Replicate:
sig
  val replicate : 'a list -> int -> 'a list
end=
struct
let nth n elm =
  let rec nth n aux=
    match n with
      _ when n>0 -> nth (n-1) (elm::aux)
      |_->aux
  in nth n [];;

let replicate l n =
  let rec replicate l aux =
    match l with
      h::t->replicate t (List.append (nth n h) aux)
      |_->List.rev aux
  in replicate l [];;
end;;
