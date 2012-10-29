(*
let rec concat = function
[] -> []
  | []::rest -> concat rest
  | (x::xs)::rest -> x::(concat (xs::rest));;

concat [[0; 3; 4]; [2]; []; [5; 0]];;
*)

let rec fold_right f l e =
  match l with
      [] -> e
    | x :: rest -> f x (fold_right f rest e);;

let rec concat l =
  let f xs = (@) xs in
  fold_right f l [];;

concat [[0; 3; 4]; [2]; []; [5; 0]];;
