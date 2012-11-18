List.fold_left;;
ListLabels.fold_left;;

ListLabels.fold_left ~f:(fun x y -> x + y) ~init:0 [1;2;3;4];;
ListLabels.fold_left ~init:0 ~f:(fun x y -> x + y) [1;2;3;4];;

type ('a, 'b) foldarg = {f: 'a -> 'b -> 'a; init: 'a};;
let rec fold_left' {f=f; init=init} = function
    [] -> init
  | a::rest -> fold_left' { f = f; init = f init a } rest;;
fold_left' { init = 0; f = (fun x y -> x + y)} [1;2;3;4];;

let g = ListLabels.fold_left ~init:0;;
g ~f:(fun x y -> x + y) [1;2;3;4];;

ListLabels.fold_left ~f:(fun x y -> x + y);;

(* definition *)
let rec fold_left ~f:func ~init:e = function
    [] -> e
  | a::rest -> fold_left ~f:func ~init:(func e a) rest;;
fold_left ~f:(fun x y -> x + y) ~init: 0 [1;2;3;4];;


let rec fold_left ~f ~init = function
    [] -> init
  | a::rest -> fold_left ~f:f ~init:(f init a) rest;;
fold_left ~f:(fun x y -> x + y) ~init: 0 [1;2;3;4];;

(* duplicated labels *)
let test ~a:x ~a:y = x - y;;

(* error *)
let empty_label ~:x = x;;

let foo ~(x:int) = x;;

ListLabels.map;;
ListLabels.map (fun x -> x + 1) [1;2;3;4];;

let k ~const ~ignored = const;;
let k' = k 1 2;;

k' ~const:(fun x y -> x) ~ignored:"hoge";;

let apply f arg1 arg2 = f ~arg1 ~arg2;;

apply (fun ~arg1 ~arg2 -> arg1 * arg2 + 1) 4 7;;
apply (fun ~arg1 ~arg2 -> arg2 * arg1 + 1) 4 7;;
(* error *)
apply (fun ~arg2 ~arg1 -> arg1 * arg2 + 1) 4 7;;

let apply_to_one f = f 1;;

apply_to_one (fun ~x -> x + x);;

(* optional arguments *)
let rec seq from ?step:(s=1) n =
  if n <= 0 then [] else from :: seq(from + s) ~step:s (n - 1);;

seq 2 5;;
seq 2 ~step:2 5;;

let rec seq from ?(step=1) n =
  if n <= 0 then [] else from :: seq(from + step) ~step:step (n - 1);;
seq 2 ~step:2 5;;

seq 3;;

(* error *)
seq 5 2 3;;

let f ?(x=1) ~y = x + y;;
f ~y:3;;

seq 1 10 ~step:4;;

(* error *)
let f = seq 1 10 in f ~step:4;;
(seq 1 10) ~step:4;;
