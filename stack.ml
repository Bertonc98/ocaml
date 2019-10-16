exception Empty_stack;;

let push s el = el::s;;

let top = function
  [] -> raise Empty_stack
  | h::t -> h;;

let pop = function
  [] -> raise Empty_stack
  | h::t -> h::t;;

let empty() = [];;
