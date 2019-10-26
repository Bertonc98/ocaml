open Pstack;;
type expr = string*string*string | string*expr*string | expr*string*string | expr*expr*string
let a = Pstack.empty();;
Pstack.push a "ciao";;

let main = print_string((Pstack.top a)^"\n");;
main
