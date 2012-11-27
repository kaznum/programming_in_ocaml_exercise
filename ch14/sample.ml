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

let l1 = `Nil
and l2 = `Cons(1, `Nil)
and l3 = `Cons(2, `Cons(1, `Nil));;

fun x -> if x then l1 else l2;;

let l4 = `Cons(true, `Cons(1, `Nil))
and l5 = `Cons(`Cons(1, `Nil));;

let rec length = function
    `Nil -> 0
  | `Cons(a, l) -> 1 + length l;;

List.map length[l1;l2;l3];;

(* error *)
length l4;;

let rec max_list = function
    `Cons(x, `Nil) -> x
  | `Cons(x, `Cons(y, l)) ->
    if x < y then max_list (`Cons(y,l)) else max_list (`Cons(x,l));;

let rec max_list = function
    `Cons(x, `Nil) -> x
  | `Cons(x, (`Cons(_, _) as l)) ->
    let m = max_list l in if x > m then x else m;;

(* error *)
let rec max = function
    `Cons(x, `Nil) -> x
  | `Cons(x, l) ->
    let m = max l in if x > m then x else m;;

let l6 = `Cons (1, `App(l2, l3));;

let rec alength = function
    `Nil -> 0
  | `Cons (a, l) -> 1 + alength l
  | `App(l1, l2) -> alength l1 + alength l2;;

alength l6;;

let rec alength_wrong = function
    (`Nil | `Cons (_, _)) as l -> length l
  | `App (l1, l2) -> alength_wrong l1 + alength_wrong l2;;
(* error *)
alength_wrong (`Cons(1, `App(l1, l2)));;

let make_length f = function
    `Nil -> 0
  | `Cons (a, l) -> 1 + f l;;

let rec length l = make_length length l;;

length l3;;

let make_alength f = function
    (`Nil | `Cons (_,_)) as l -> make_length f l
  | `App (l1, l2) -> f l1 + f l2;;

let rec alength l = make_alength alength' l;;

alength l3;;
alength (`Cons(1, `App(l2, l3)));;
(* alengthは、lengthには依存していない点に注意。lengthの定義は無くても動作する *)

type 'a mylist = [`Nil | `Cons of 'a * 'a mylist];;

let make_alength f = function
    #mylist as l -> make_length f l
  | `App(l1, l2) -> f l1 + f l2;;

let rec alength l = make_alength alength l;;
(* `Appが l の型に現れないのでダメ *)

type ('a, 'b) mylist = [`Nil | `Cons of 'a * 'b];;
let make_alength f = function
    #mylist as l -> make_length f l
  | `App(l1, l2) -> f l1 + f l2;;
let rec alength l = make_alength alength l;;

alength l3;;
alength (`Cons(1, `App(l2, l3)));;


(* 14.4 *)
(* error *)
function `A x -> x + 1 | `A y -> int_of_float y + 2 | `B -> 2;;

let f = function `A x -> x + 1 | `B -> 2;;
let g = function `A y -> int_of_float y + 2 | `B -> 3;;
let f_or_g = fun x -> if x then f else g;;

(* error:: mis-spell Autumn -> Autum *)
let next_season = function
    `Sprint -> `Summer | `Summer -> `Autumn | `Autum -> `Winter;;

next_season `Autumn;;

(* column *)
let f o = [o#foo 1; o#bar "0xAB"];;
let g v = match v with `Foo x -> x + 1 | `Bar y -> int_of_string y;;

let l = [`Foo 1; `Bar "0xAB"];;
List.map g l;;

f (object
  method foo x = x + 1
  method bar y = int_of_string y
end);;
