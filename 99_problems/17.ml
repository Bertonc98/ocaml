let split l n =
  if n>=(List.length l) then [l;[]]
  else
    let rec split l aux1 aux2 i =
      match l with
        []->[(List.rev aux1);(List.rev aux2)]
        |h::t when i<n -> split t (h::aux1) aux2 (i+1)
        |h::t when i>=n -> split t aux1 (h::aux2) (i+1)
    in split l [] [] 0;;
