let x = read_int();;
let fact z=
  let rec fact2 y accum =
    match y with
      1->accum
      |_->fact2 (y-1) (y*accum)
in fact2 z 1;;

let main() = print_string(string_of_int (fact x)^"\n");;;;

main();;


(*La funzione fact è la funzione chiamante, per effettuare la ricorsione in coda, senza far crescere lo stack
Come funziona?
Ogni chiamata viene fatta da quella funzione, a cui tutte le chiamate ritorneranno, stack altezza limitata
*)
