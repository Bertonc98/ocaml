open Char;;
let file = "testo.txt"
let input_channel = open_in file;;

let is_alpha c =
  match c with
    'a'..'z' -> true
    |_->false

let read_word ic =
  let rec read_word ic w =
      let c=lowercase_ascii (input_char ic)
        in
        match is_alpha c with
          true->read_word ic (String.concat "" [w; (String.make 1 c)])
          |_-> w
  in read_word ic "";;

type occurence = {parola:string; count:int};;

let is_in w l =
  let rec is_in w l=
    match l with
      []-> [{parola=w;count=1}]
      |h::t->match h.parola with
             _ when w=h.parola->[{parola=w;count=(h.count+1)}]
             |_->is_in w t
  in is_in w l;;

let read_text ic =
  let rec read_text ic txt =
    try
      let w= read_word ic
      in
        match w with
          _->read_text ic (List.append (is_in w txt) txt)
    with
    End_of_file-> List.filter (fun x->match x.parola with ""->false |_->true) txt
  in read_text input_channel [];;

let occorrenze = read_text input_channel;;
