let is_prime n =
  let rec is_prime n aux =
    if n<>2 && (n mod 2)=0 then false
    else
    match n with
      _ when (n mod aux)<>0 -> is_prime n (aux-1)
      |_ when aux=1 -> true
      |_->false
  in is_prime n (n/2);;

exception NotEven;;

let goldbach n =
  if ((n mod 2)<>0) || n<=2 then raise NotEven
  else
  let rec goldbach n aux =
    match n with
      _ when ((is_prime aux) && (is_prime (n-aux))) -> (aux, (n-aux))
      |_ -> goldbach n (aux+1)
  in goldbach n 2;;

let goldbach_list n m=
  let rec goldbach_list current aux=
    if current <= (max n m) then
      (if (current mod 2)=0 then goldbach_list (current+1) ((goldbach current)::aux)
      else goldbach_list (current+1) aux)
    else aux
  in List.rev (goldbach_list (min n m) []);;
