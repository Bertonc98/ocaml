let encode l =
  let rec encode l aux count=
    match l with
      []->aux
      |[x]->((count+1),x)::aux
      |a::(b::_ as t)->if a=b then encode t aux (count+1)
                  else encode t ((count+1,a)::aux) 0
  in List.rev (encode l [] 0);;
