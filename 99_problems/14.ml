let duplicate l =
  let rec duplicate l aux =
    match l with
      h::t->duplicate t (h::(h::aux))
      |_->List.rev aux
  in duplicate l [];;
