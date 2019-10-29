let rotate l n =
  let rec rotate l mx i aux =
    match l with
      h::t when i<mx -> rotate t mx (i+1) (List.append aux [h])
      |h::t when i>=mx->List.append (h::t) aux
  in rotate l ((fun x -> if x < 0 then (((List.length l)+(x mod (List.length l))) mod (List.length l)) else (x mod (List.length l)) ) n) 0 []
