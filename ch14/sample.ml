`Can;;
`May;;
`Can 2;;
`April ("fool", false);;

fun x -> if x then `Can else `May;;
[`Can; `May];;

[`Can 2; `Bottle; `Can 3];;

let c = `Can and m = `May and b = `Bottle;;

[c; m];;

[c; b];;

(* error *)
fun x -> if x then `Can 2 else `Can true;;
[`Can 2; `Can true];;

(* success *)
(`Can 2, `Can true);;

let kani = function
    `Right -> "walking to the right"
  | `Left -> "walking to the left";;

let hito = function
    `Right -> "walking to the right"
  | `Left -> "walking to the left"
  | `Forward -> "walking forward"
  | `Backward -> "walking backward";;

(* error *)
let c : [`Can] = `Can and m = `May in
[m; c];;
let nil : int list = [] in
true :: nil;;

[kani; hito];;


let mirror = function
    `Right -> `Left
  | `Left -> `Right
  | x -> x;;

let mirror' = function
    `Right -> `Left
  | `Left -> `Right;;

let add_A x = `A :: x;;

add_A [`B];;

(* error *)
add_A [`A 1];;
