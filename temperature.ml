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
