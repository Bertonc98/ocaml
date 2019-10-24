let rec map f = function
  []->[]
  |h::t->f h :: map f t

let rec filter f = function
  []->[]
  |h::t->if f h then h::filter f t else filter f t

let rec reduce acc op = function
  []->acc
  |h::t->reduce (op h acc) op t
