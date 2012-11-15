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
