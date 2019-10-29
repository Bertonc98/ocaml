let rand_select l n =
  Random.init;
  let rec rand_select l n aux =
    let r = Random.int (List.length l)
    in
    if n=0 then aux else rand_select (List.filter (fun x -> x<>(List.nth l r)) l) (n-1) ((List.nth l r)::aux)
  in rand_select l n [];;
