(* first implementation *)
let sum a b = a+b;;
let dif a b = a-b;;

let range a b =
  let op = if a<=b then sum else dif
  in
    let rec range aux i =
      if i=b then List.rev (i::aux)
      else range (i::aux) (op i 1)
    in range [] a;;

(* solution implementation *)
let range a b =
  let rec range aux high low =
    if high>low then
      range (high::aux) (high-1) low
    else aux
  in
    if a<b then List.rev (range [] b a) else range [] a b
