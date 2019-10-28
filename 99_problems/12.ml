type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let create n c =
  let rec create n aux =
    match n with
      _ when n>0 -> create (n-1) (c::aux)
      |_->aux
  in create n [];;

let decode l =
  let rec decode l aux =
    match l with
      h::t-> (match h with
               One c-> decode t (c::aux)
               |Many (n,c)-> decode t (List.append (create n c) aux))
      |_->aux
    in List.rev(decode l []);;
