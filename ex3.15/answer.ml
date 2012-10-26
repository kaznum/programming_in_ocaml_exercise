(* int -> int -> int -> int *)
let sum_1 = fun a b c ->
  a + b + c;;
sum_1 1 2 3;;

let sum_2 = fun f a ->
  f (a + 1) + 2;;

sum_2 ((+) 1) 2;; (* (+) 1 (2 + 1) + 2 *)

let sum_3 = fun f ->
  f 1 2 + 3;;

sum_3 ( + );;
sum_3 ( * );;
