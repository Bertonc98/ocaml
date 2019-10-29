let insert_at elm n l =
  if n>(List.length l) then l
  else
    let rec insert_at l aux i =
      match l with
        h::t when i<>n ->insert_at t (h::aux) (i+1)
        |h::t->List.append (List.rev (h::(elm::aux))) t
        |[]->List.append (List.rev aux) [elm]
    in insert_at l [] 0
