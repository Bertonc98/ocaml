let find list el =
  let rec find_rec list el =
    match list with
      [] -> false
      |head::tail-> if head==el then true else find_rec tail el
in find_rec list el;;

let count list el =
  let rec count_rec el count = function
    [] -> count
    | head::tail -> if head==el then count_rec el (count+1) tail else count_rec el count tail
in count_rec el 0 list;;

let slice list start en =
  let rec slice_rec start en index = function
    [] -> []
    | head::tail -> if index<start || index>en then slice_rec start en (index+1) tail
                    else head::slice_rec start en (index+1) tail
  in slice_rec start en 0 list;;


let a = [1;4;7;3;7;7];;

count a 7;;

print_string("\n\n\n");;
