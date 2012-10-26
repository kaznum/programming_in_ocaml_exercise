let rec sum_list = function
[] -> 0
  | n :: rest -> n + (sum_list rest);;

sum_list [1; 2; 3];;
sum_list (1::2::3::[]);;

let rec length = function
[] -> 0
  | _ :: rest -> 1 + length rest;;
length [1; 2; 3];;

let rec append l1 l2 =
  match l1 with
      [] -> l2
    | v :: l1' -> v :: (append l1' l2);;
append [1;2;3] [4;5;6];;

[1;2;3] @ [4;5;6];;

(* the following definition is not effective *)
let rec reverse = function
[] -> []
  | v :: l' -> reverse l' @ [v];;
reverse [];;
reverse [1;2;3];;

(* the following definition is more effective then reverse above *)
let rec revAppend l1 l2 =
  match l1 with
      [] -> l2
    | v :: l1' -> revAppend l1' (v :: l2)
let rev l = revAppend l [];;

rev [1;2;3];;

let rec map f = function
[] -> []
  | x :: rest -> f x :: map f rest;;

map (fun x -> x * 2) [1;2;3;4];;


let rec forall f = function
[] -> true
  | x :: rest -> (f x) && (forall f rest);;
forall ( fun x -> x >= 5) [7;10;8];;
forall ( fun x -> x >= 5) [9;3;8];;

let rec exists f = function
[] -> false
  | x :: rest -> (f x) || (exists f rest);;

exists ( fun x -> x >= 5) [9;3;8];;
exists ( fun x -> x >= 5) [2;3;4];;

let rec fold_right f l e =
  match l with
      [] -> e
    | x :: rest -> f x (fold_right f rest e);;

let rec fold_left f e l =
  match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest;;

fold_right ( fun x y -> x + y ) [ 3; 5; 7] 0;;
fold_left (fun x y -> y :: x) [] [1; 2; 3;];;

let length_foldr l = fold_right (fun x y -> 1 + y) l 0;;
length_foldr [1;2;3;4;5];;

let rec nth n l =
  match (n, l) with
      (1, a :: _) -> a
    | (n', _ :: rest) when n' > 0 -> nth (n-1) rest;;

nth 3 [1;4;9;16];;


let city_phone = [("Kyoto", "075"); ("Tokyo", "03"); ("Sapporo", "011")];;
let rec assoc a = function
(a', b) :: rest -> if a = a' then b else assoc a rest;;

assoc "Tokyo" city_phone;;

(* sample of wrong way
let rec assoc_error a = function
(a, b) :: rest -> b
  | _ :: rest -> assoc_error a rest;;

assoc_error "Tokyo" city_phone;;
*)
