let rev l =
  let rec rev l aux =
    match l with
    []->aux
    |h::t->rev t (h::aux)
  in rev l [];;
