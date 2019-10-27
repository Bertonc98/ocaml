exception OutOfBound;;

let at pos l =
  let rec at pos l i =
    match l with
      []->raise OutOfBound
      |h::t when i=pos -> h
      |h::t -> at pos t (i+1)
  in at pos l 1;;
