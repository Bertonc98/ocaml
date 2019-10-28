type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode l =
  let rec encode l aux count=
    match l with
      []->aux
      |[x]->if count = 0 then ((One x)::aux)
            else ((Many (count+1,x))::aux)
      |a::(b::_ as t)->if a=b then encode t aux (count+1)
                  else if count = 0 then encode t ((One a)::aux) 0
                  else encode t ((Many (count+1,a))::aux) 0
  in List.rev (encode l [] 0);;
