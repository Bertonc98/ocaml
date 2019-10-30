module Kwic:
sig
  val input_channel : unit -> in_channel
  val read_titles : in_channel -> (string * int) list
  val permute : (string * 'a) list -> ('a * string list * string list) list
  val get_sorted_permutation : unit -> (int * string list * string list) list
end =
struct
  open Char;;
  let file = "titles.txt"
  let input_channel() = open_in file;;

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

  let rotation s n =
    let stringa = List.filter (fun x->(String.length x)>2) (List.filter (fun x->(x<>" ")) (String.split_on_char ' ' s))
    in
      let rec rotation aux pre post =
        match post with
          h::t -> if (((String.lowercase_ascii h)<>"the") && ((String.lowercase_ascii h)<>"and"))
                  then rotation ((n,(List.rev pre),(post))::aux) (h::pre) t
                  else rotation aux (h::pre) t
          |_->aux
      in rotation [] [] stringa

  let permute l =
    let rec permute l aux=
      match l with
        (s,n)::t-> permute t (List.append aux (rotation s n))
        |_->aux
    in permute l []

  let get_post a =
    match a with
      (_,_,h::t)->h::t;;

  let compare_perm a b =
    match a with
      (_,_,h::t) ->


  let get_sorted_permutation() = List.sort compare_perm (permute (read_titles (input_channel())))


end;;
