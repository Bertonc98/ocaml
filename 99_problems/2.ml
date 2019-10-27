exception VoidList;;
exception SingleList;;

let rec penultimi lista =
  match lista with
  []->raise VoidList
  |[x]->raise SingleList
  |[x;y]->(x,y)
  |_::t->penultimi t;;

penultimi [];;
penultimi ["a"];;
let l =["a";"b";"c";"d"];;
