let len l =
  let rec len l i =
    match l with
      []->i
      |h::t -> len t (i+1)
  in len l 0;;
