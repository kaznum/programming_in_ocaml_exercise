type nat = Zero | OneMoreThan of nat;;
let zero = Zero
and one = OneMoreThan Zero;;
let two = OneMoreThan one;;

let rec monus m n =
  match (m, n) with
      (m', Zero) -> m'
    | (m', n') when m' = n' -> Zero
    | (m', OneMoreThan n') when monus m' n' = Zero -> Zero
    | (m', n) -> OneMoreThan (monus m (OneMoreThan n));;

monus two Zero;;
monus one one;;
monus two Zero;;
monus Zero two;;
monus two one;;
monus one two;;
monus two two;;
monus two (OneMoreThan two);;
