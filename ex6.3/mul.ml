type nat = Zero | OneMoreThan of nat;;
let zero = Zero
and one = OneMoreThan Zero;;
let rec add m n =
  match m with
      Zero -> n
    | OneMoreThan m' -> OneMoreThan (add m' n);;

let rec mul m n =
  match m with
      Zero -> Zero
    | OneMoreThan m' -> add (mul m' n) n;;

let two = OneMoreThan one;;
mul two Zero;;
mul two one;;
mul one two;;
mul two two;;
mul two (OneMoreThan two);;
