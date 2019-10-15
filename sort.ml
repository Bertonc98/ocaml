let rec sort list=
  match list with
    []->[]
    | head::tail-> insert head (sort tail)
  and
  insert element lst=
    match lst with
      []->[element]
      |head :: tail-> if element<=head then element::lst else head :: insert element tail
;;

let x = [2;7;3;1;4];;

sort x;;
