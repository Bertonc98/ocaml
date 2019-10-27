type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flatten l =
  let rec flatten l aux=
    match l with
      []-> aux
      |h::t->match h with
              One h->flatten t (aux@[h])
              |Many h->flatten t (flatten h (aux))
  in flatten l [];;
