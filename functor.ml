type comparison = Less | Equal | Greater;;

module type ORDERED_TYPE=
  sig
      type t
      val compare : t -> t -> comparison
  end;;

module OrderedString =
  struct
    type t = string
    let compare x y = if x = y then Equal else if x > y then Greater else Less
  end;;

module Set =
  functor (Elt : ORDERED_TYPE) ->
    struct
      type element = Elt.t
      type set = element list
      let empty = []
      let rec member x s=
        match s with
          []->false
          | hd::tl -> match Elt.compare hd x with
                        Equal -> true
                        | Less -> false
                        | Greater -> member x tl
    end;;

module StringSet = Set(OrderedString);;
