let slice l i k =
  let rec slice l count aux =
    match l with
      h::t when (i<=count && count<=k)->slice t (count+1) (h::aux)
      |h::t->slice t (count+1) aux
      |_->List.rev aux
  in slice l 0 [];;
