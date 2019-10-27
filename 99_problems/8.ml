let compress l =
  let rec compress l last aux =
    match l with
      []->List.rev aux
      |h::t-> if last<>h then compress t h (h::aux)
              else compress t h aux
  in compress l "" [];;
compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
