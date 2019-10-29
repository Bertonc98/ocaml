let drop l n =
  let rec drop l aux i=
    match l with
      h::t when n<>i -> drop t (h::aux) (i+1)
      |h::t when n=i -> drop t aux (i+1)
      |[]->List.rev aux
  in drop l [] 0;;
