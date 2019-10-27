let is_in

let encode l =
  let encode l aux =
    match l with
      []->aux
      |h::t-> 
