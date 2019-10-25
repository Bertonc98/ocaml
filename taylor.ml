let fact x =
  let rec fact x acc =
    match x with
    _ when x>0->fact (x-1) (acc*x)
    |_->acc
  in fact x 1;;

let op x i =
let n = (float_of_int x)**(float_of_int i) and
d = (float_of_int(fact i))
in
n/.d;;

let sin x n =
  let rec sin x n i acc o=
    match i with
      _ when i<=n -> if ((i mod 2)=1) then if o="+" then sin x n (i+1) (acc +. (op x i)) "-"
                                                    else sin x n (i+1) (acc -. (op x i)) "+"
                    else sin x n (i+1) acc o
      |_->acc
  in sin x n 1 0. "+";;
