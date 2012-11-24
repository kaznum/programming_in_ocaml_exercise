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

let singleton_A x =
  let _ = `A :: x in
  [`A];;

singleton_A [`B];;
(singleton_A : [`A | `B] list -> [`A | `C] list);;

(* error *)
(add_A : [`A | `B] list -> [`A | `C] list);;


let c = ref [`Can];;
c := (`May : [`Can | `May]) :: !c;;
c;;
(* error *)
c := `Will :: !c;;


let c = ref [];;
c := 1 :: !c;;
c;;
(* error *)
c := true :: !c;;


type seasons = [ `Spring | `Summer | `Autumn | `Winter];;
(* error *)
type seasons = [> `Spring | `Summer | `Autumn | `Winter];;

type seasons = [ `Spring | `Summer | `Autumn | `Winter];;
type seasons_of_japan = [ seasons | `Tsuyu];;

let hito' = function
    `Forward -> "walking forward"
  | `Backward -> "walking backward"
  | (`Right | `Left) as x -> kani x;;

hito' `Right;;

let hito' = function
    `Forward -> "walking forward"
  | `Backward -> "walking backward"
  | x -> kani x;;

type kani_dir = [`Left | `Right];;
let hito' = function
    `Forward -> "walking forward"
  | `Backward -> "walking backward"
  | #kani_dir as x -> kani x;;


type sample_var = [`Hoge of int * string | `Fuga of string * int];;
let hito'' = function
    `Forward -> "walking forward"
  | `Backward -> "walking backward"
  | #kani_dir as x -> kani x
  | #sample_var -> "sample_var";;
