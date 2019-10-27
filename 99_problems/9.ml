let pack l =
  let rec pack l curr aux =
    match l with
      []->List.rev aux
      |[x]->(x::curr)::aux
      |h::(m::_ as t)-> if h<>m then pack t [] ((h::curr)::aux)
                   else pack t (h::curr) aux
  in pack l [] [];;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
