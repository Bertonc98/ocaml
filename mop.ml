module Mop:
  sig
    val matrix_equivalence : int array array -> int array array -> bool
    val matrix_copy : int array array -> int array array -> unit
    val ( + ) : int array array -> int array array -> int array array
    val matrix_scalar_mul : int -> int array array -> int array array
    val matrix_trasposition : int array array -> int array array
    val print_matrix : int array array -> unit
  end=
  struct

    let get_sizes m = ((Array.length m),(Array.length m.(0)));;

    exception NotEqual;;

    exception ImpossibleOperation;;

    let matrix_equivalence a b =
      if (get_sizes a)<>(get_sizes b) then false
      else
        try
          let cmp_row i row =
            Array.iteri (fun x _ -> if (a.(i).(x))<>(b.(i).(x)) then raise NotEqual else () ) row
            in
              Array.iteri (cmp_row) a; true
        with
        | _ -> false

    let matrix_copy a b =
      if (get_sizes a)<>(get_sizes b) then raise ImpossibleOperation
      else
        let copy_row i _ =
          Array.iteri (fun x _ -> b.(i).(x)<-a.(i).(x)) a.(i)
        in
        Array.iteri (copy_row) a;;

    let make_skeleton (i,j) = Array.make_matrix i j 0;;

    let ( + ) a b =
      if (get_sizes a)<>(get_sizes b) then raise ImpossibleOperation
      else
        let c = make_skeleton(get_sizes a)
        in
          let sum_elm i j _ =
            c.(i).(j)<-((a.(i).(j))+(b.(i).(j)))
          in
            let sum_row i _ =
              Array.iteri (sum_elm i) a.(i)
            in
              Array.iteri (sum_row) a; c;;

    let matrix_scalar_mul k m =
        let c = make_skeleton(get_sizes m)
        in
          Array.iteri (fun x _ -> (Array.iteri (fun y _ -> c.(x).(y)<-(m.(x).(y)*k)) m.(x)) ) m; c;;

    let matrix_trasposition m =
      let m_size = get_sizes m
      in
      match m_size with
        (a,b) when (a=1) && (b=1) ->m
        |(a,b)->
               let c = make_skeleton (b,a)
               in
                 let swap x y _= c.(x).(y)<-m.(y).(x)
                 in
                   Array.iteri (fun i _ -> (Array.iteri (swap i) c.(i))) c; c;;

    let print_matrix m =
      let print i j _ = Printf.printf "%d\t" m.(i).(j)
      in
        let print_row i _ = (Array.iteri (print i) m.(i)); Printf.printf "\n"
        in
          Array.iteri (print_row) m;;
end;;
