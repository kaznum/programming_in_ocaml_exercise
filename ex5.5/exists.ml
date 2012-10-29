(*
let rec exists f = function
[] -> false
  | x :: rest -> (f x) || (exists f rest);;
*)

let rec fold_right f l e =
  match l with
      [] -> e
    | x :: rest -> f x (fold_right f rest e);;

let rec forall f l =
  fold_right (fun x -> (||) (f x)) l false;;

exists ( fun x -> x >= 5) [9;3;8];;
exists ( fun x -> x >= 5) [2;3;4];;
