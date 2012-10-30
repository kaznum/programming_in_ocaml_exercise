type nat = Zero | OneMoreThan of nat;;
let zero = Zero
and one = OneMoreThan Zero;;
let two = OneMoreThan one;;

let rec minus m n =
  match (m, n) with
      (m', Zero) -> Some m'
    | (m', n') when m' = n' -> Some Zero
    | (m', OneMoreThan n') when minus m' n' = Some Zero || minus m' n' = None -> None
    | (m', n') ->
      match minus m' (OneMoreThan n') with
	  None -> None
	| Some v -> Some (OneMoreThan v);;


minus two Zero;;
minus one one;;
minus two Zero;;
minus Zero two;;
minus two one;;
minus one two;;
minus two two;;
minus two (OneMoreThan two);;
