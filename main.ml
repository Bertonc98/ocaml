open StackABS;;

module Interi = struct type t = int end;;
module Stringhe = struct type t = string end;;
module Liste = struct type t =int list end;;

module I = StackABS(Interi);;
module S = StackABS(Stringhe);;
module L = StackABS(Liste);;

let a = S.empty();;
let a = S.push a "World\n";;
let a = S.push a "Hello\n";;

print_string(S.pop a);;
print_string(S.pop a);;


