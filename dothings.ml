let pow = fun x -> x*x;;

let dothings f x = f(x);;

print_string(string_of_int(dothings pow 3)^"\n");;
