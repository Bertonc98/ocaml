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

    let prod_matrix m1 m2=
      let m3 = zeros (Array.length m1) (Array.length m2.(0))
      in m3;;



    let ( * ) m1 m2=
      if (Array.length m1.(0))<>(Array.length m2) then raise Impossible_Product
      else
      let m3 = prod_matrix m1 m2
      in
        let prod_r_c  m1_r_index m2_c_index _=
          let sum x _= m3.(m1_r_index).(m2_c_index)<-m3.(m1_r_index).(m2_c_index)+(m1.(m1_r_index).(x))*(m2.(x).(m2_c_index))
          in
            Array.iteri sum m1.(m1_r_index)
            in
              let row row_index _= Array.iteri (prod_r_c row_index) m3.(row_index)
              in
                Array.iteri row m3;m3;;

end;;



let a=Matrix.zeros 3 2;;
let b=Matrix.zeros 2 3;;
a.(0).(0)<-1;;
a.(1).(1)<-1;;
a.(2).(1)<-1;;
b.(0).(0)<-2;;
b.(1).(1)<-2;;
b.(1).(2)<-2;;
let c=Matrix.zeros 3 3;;
Matrix.print_matrix a;;
print_string("\n\n");;
Matrix.print_matrix b;;
print_string("\n\n");;
Matrix.print_matrix (Matrix.( * ) a b);;
