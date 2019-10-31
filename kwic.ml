module Kwic:
sig
  val input_channel : unit -> in_channel
  val read_titles : in_channel -> (string * int) list
  val permute : (string * 'a) list -> ('a * string list * string list) list
  val get_sorted_permutation : unit -> (int * string list * string list) list
  val print : (int * string list * string list) list -> unit
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
    in permute l [];;

  let rev_s s =
    let len = String.length s in
    String.init len (fun i->s.[len-1-i]);;

  let to_string l =
    let rec to_string aux l=
      match l with
        h::t->to_string ((rev_s h)^" "^aux) t
        |_->rev_s aux
    in String.trim (to_string "" l);;

  let get_post a =
    match a with
      (_,_,h::t)->to_string (h::t);;

  let get_pre a =
    match a with
      (_,h::t,_)->to_string (h::t)
      |(_,[],_)-> "";;

  let get_line a =
    match a with
      (n,_,_)->string_of_int n;;

  exception InvalidPermutation;;

  let compare_perm a b =

    match a with
      (_,_,(ha::ta)) -> (match b with
                          (_,_,(hb::tb)) -> if (get_post(a))=(get_post(b)) then 0
                                          else if (get_post(a))>(get_post(b)) then 1
                                          else -1)
      |_->raise InvalidPermutation;;



  let get_sorted_permutation() = List.sort compare_perm (permute (read_titles (input_channel())));;

  exception TooMuchLines;;

  let print_line n =
    if (String.length n)>5 then raise TooMuchLines
    else
    let rec print_line aux count =
      if count >0 then print_line (" "^aux) (count-1)
      else aux
    in print_line n (5-(String.length n));;

  let n_spaces n =
    let rec n_spaces aux count =
      if count>0 then n_spaces (" "^aux) (count-1)
      else aux
    in n_spaces "" n;;

  let print_pre pre =
    if (String.length pre)>33 then (String.sub pre 0 33)
    else
      (n_spaces (33-(String.length pre)))^pre;;

  let print_post post =
    if (String.length post)>40 then (String.sub post 0 40)
    else
      post^(n_spaces (40-(String.length post)));;

  let rec print tuple=
    match tuple with
      h::t->print_string(print_line (get_line h)^" "^print_pre (get_pre h)^" "^print_post (get_post h)^"\n"); print t
      |_->();;

end;;
