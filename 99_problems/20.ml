let remove_at n l =
  if n>=(List.length l) then l
  else
    let rec remove_at l aux i =
      match l with
        h::t when i<>n ->remove_at t (h::aux) (i+1)
        |h::t->remove_at t aux (i+1)
        |_->List.rev aux
    in remove_at l [] 0
