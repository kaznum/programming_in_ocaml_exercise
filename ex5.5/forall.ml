(*
let rec forall f = function
[] -> true
  | x :: rest -> (f x) && (forall f rest);;
*)

let rec fold_right f l e =
  match l with
      [] -> e
    | x :: rest -> f x (fold_right f rest e);;

let rec forall f l =
  fold_right (fun x -> (&&) (f x)) l true;;

forall ( fun x -> x >= 5) [7;10;8];;
forall ( fun x -> x >= 5) [9;3;8];;
