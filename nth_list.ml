exception Not_valid_number
let a = [1;2;3;4;5;6;7]
let nth n l =
  let rec nth acc n l =
    match l with
      [] -> raise Not_valid_number
      | h::t -> if acc=n then h else nth (acc+1) n t
  in nth 0 n l

let lung l =
  let rec lung acc l =
    match l with
    [] -> acc
    |h::t->lung (acc+1) t
  in lung 0 l

let rev l =
  let rec rev acc l =
    match l with
      []->acc
      |h::t->rev (h :: acc) t
  in rev [] l
