exception VoidList;;

let rec last lista =
  match lista with
  []->raise VoidList
  |[x]->x
  |_::t->last t;;

last [];;
let l =["a";"b";"c";"d"];;
