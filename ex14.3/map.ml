(* the definition of map *)
let rec map f x =
  match x with
      `Nil -> `Nil
    | `Cons(a, b) -> `Cons(f a, map f b);;

map (( * ) 2) `Nil;;
map (( * ) 2) (`Cons(1, `Nil));;
map (( * ) 2) (`Cons(2, (`Cons(4, `Nil))));;


(* the definition of amap *)
let rec list_of_alist alist =
  let rec val_list als =
    match als with
	`Nil -> []
      | `Cons (x, y) -> x :: val_list y
      | `App (x, y) -> val_list x @ val_list y in
  let rec list_of_vals vals =
    match vals with
	[] -> `Nil
      | x::xs -> `Cons(x, list_of_vals xs) in
  list_of_vals (val_list alist);;

let amap f al =
  let l = list_of_alist al in
  map f l;;

(* test *)
let l1 = `Nil
and l2 = `Cons(1, `Nil)
and l3 = `Cons(2, `Cons(1, `Nil));;

let l4 = `App (l2, l3);;
let l5 = `App(l4, `Cons(5, `Cons(2, `Nil)));;

amap (( * ) 2) l1;;
amap (( * ) 2) l2;;
amap (( * ) 2) l3;;
amap (( * ) 2) l4;;
amap (( * ) 2) l5;;
