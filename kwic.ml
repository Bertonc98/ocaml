open Char;;
let file = "titles.txt"
let input_channel = open_in file;;

let continue c =
  match c with
    '\n' -> false
    |_->true

let read_line ic =
  let rec read_line ic w =
      let c=input_char ic
        in
        match continue c with
          true->read_line ic (String.concat "" [w; (String.make 1 c)])
          |_-> w
  in read_line ic "";;

let read_titles ic =
  let rec read_titles ic l x=
    try
      let line = read_line ic
      in
      match line with
        _-> read_titles ic (((String.trim line), x)::l) (x+1)
    with
    | End_of_file -> List.rev l
  in read_titles ic [] 1
