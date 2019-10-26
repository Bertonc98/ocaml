module Matrix =
  struct
    let zeros n m = Array.make_matrix n m 0

    let print_matrix matrix =
      let print_element elm=
        Printf.printf "%i\t" elm
      in
        let print_row row =
          Array.iter print_element row; print_endline ""
        in
          Array.iter print_row matrix;;

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

    let t_matrix matrix =
      let x = Array.length matrix
      and y = Array.length matrix.(0)
      in
      Array.make_matrix y x matrix.(0).(0);;

    let transpose m =
      let m' = t_matrix m
      in
        let swap i j _=
          m'.(j).(i)<-m.(i).(j)
        in
          let swap_row row_index row=
            Array.iteri (swap row_index) row
          in
            Array.iteri swap_row m;m';;

    exception Impossible_Product;;

    let ( * ) m1 m2=
      if (Array.length m1.(0))<>(Array.length m2) then raise Impossible_Product
      else
        print_string("Va\n");;
end;;



let a=Matrix.zeros 3 2;;
let b=Matrix.zeros 2 3;;
( * ) a b;;
