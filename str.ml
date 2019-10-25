let palindrome w =
  let len = String.length w
  in
    let rec palindrome s e w =
      match w with
        _ when s<e -> if w.[s]<>w.[e] then false
                      else palindrome (s+1) (e-1) w
        |_->true
  in palindrome 0 (len-1) w;;

let is_in c w =
  let len = (String.length w)
  in
  let rec is_in c w n=
    match c with
      _ when n<len -> if w.[n]=c then true else is_in c w (n+1)
      |_->false
  in is_in c w 0;;

let (-) w1 w2 =
  let len = String.length w1
  in
  let rec (-) w1 w2 n str=
    match n with
    _ when n<len -> if is_in w1.[n] w2 then (-) w1 w2 (n+1) str
                    else (-) w1 w2 (n+1) (str^(String.make 1 w1.[n]))
    |_->str
  in (-) w1 w2 0 "";;

let compare w1 w2 =
  if ((-) w1 w2)="" then true
  else false

let anagram w dict =
  let rec anagram w dict =
    match dict with
    []->false
    |h::t-> if compare h w then true else anagram w t
  in anagram w dict;;
