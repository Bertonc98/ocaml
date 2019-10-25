(* beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88) *)

type element = {name:string ; atomic:int}

let alkaline_earth_metals = [{name="beryllium";atomic=4};
                             {name="calcium";atomic=20};
                             {name="magnesium";atomic=12};
                             {name="barium";atomic=56};
                             {name="strontium";atomic=38};
                             {name="radium";atomic=88}]
let high_number l =
  let rec high_number m l=
    match l with
      []->m
      |h::t->if h.atomic > m then high_number (h.atomic) t else high_number m t
  in high_number 0 l;;

(*let execute() = print_string(string_of_int(high_number alkaline_earth_metals)^"\n");;

execute();; *)
let minor a b =
  if a.atomic<b.atomic then true
  else false

let mag a b =
  if a.atomic>b.atomic then true
  else false

let sort l =
  let rec sort l =
    match l with
      []->[]
      | h::t -> sort (List.filter (fun x->minor x h) t)
                @ [h] @
                sort (List.filter (fun x->mag x h) t)
in sort l;;
(* sort alkaline_earth_metals;; *)

let noble_gas =[{name="helium";atomic=2};
                {name="neon";atomic=10};
                {name="argon";atomic=18};
                {name="krypton";atomic=36};
                {name="xenon";atomic=54};
                {name="radon";atomic=86}]

let compare_element a b =
  if a.name<b.name then -1
  else if a.name=b.name then 0
  else 1


let print_lista l1 l2=
  let rec print_lista l =
    match l with
      h::t->print_string(h.name^" "^(string_of_int h.atomic)^"\n"); print_lista t
      |_->print_string("\n")
  in print_lista (List.sort compare_element (l1@l2));;

(* print_lista alkaline_earth_metals noble_gas;; *)

let zeros n m = Array.make_matrix n m 0;;

let identity n =
  let rec identity acc n base =
    match acc with
    _ when acc<n -> base.(acc).(acc)<-1; identity (acc+1) n (base)
    |_->base
  in identity 0 n (zeros n n);;

let rec set j i n base =
  match n with
    _ when j<n -> base.(j)<-((i*n)+j); set (j+1) i n base
    |_->base

let init n=
  let rec init i j n base =
    match i with
      _ when i<n -> base.(i)<-set j i n base.(i); init (i+1) 0 n base
      |_->base
  in init 0 0 n (zeros n n);;

let rec copy_set i j x y base base' =
  match i with
    _ when j<y -> base'.(j).(i)<-base.(i).(j); copy_set i (j+1) x y base base'
    |_->base'

type scale = Kelvin | Celsius | Fahrenheit | Rankine | Delisle | Newton | Reaumur | Romer

let scales = [Kelvin ; Celsius ; Fahrenheit ; Rankine ; Delisle ; Newton ; Reaumur ; Romer];;

let to_kelvin (s, v) =
  match s with
    Kelvin->(Kelvin, v)
    |Celsius->(Celsius, (v+.273.15))
    |Fahrenheit->( Fahrenheit, (v+.209.15))
    |Rankine-> ( Rankine, (v-.250.56))
    |Romer->( Romer, (v+.284.65))
    |Newton->( Newton, (v+.299.95))
    |Delisle->( Delisle, (v+.223.15))
    |Reaumur->( Reaumur, (v+.281.15))

let from_kelvin (t, v) =
  match t with
    Kelvin->( Kelvin, v)
    |Celsius->( Celsius, (v-.273.15))
    |Fahrenheit->( Fahrenheit, (v-.209.15))
    |Rankine-> ( Rankine, (v+.250.56))
    |Romer->( Romer, (v-.284.65))
    |Newton->( Newton, (v-.299.95))
    |Delisle->( Delisle, (v-.223.15))
    |Reaumur->( Reaumur, (v-.281.15))

let translate_from (s, v) =
  List.map from_kelvin (List.map (fun x->(x,v)) (List.filter (fun x -> x<>s)scales))
  (*List.map to_kelvin (List.map (fun x -> (x, v) )scales) *)

let table v =
List.map translate_from(List.map (fun x -> (x,v)) scales)
