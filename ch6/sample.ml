type student = {name : string; id : int};;
let st1 = {name = "Taro Yamada"; id = 123456}
and st2 = {id = 51; name = "Ichiro Suzuki"};;

let string_of_student { name = n; id = i} = n ^ "'s ID is " ^ string_of_int i;;

string_of_student st1;;

let name_of_student { name = n } = n;;
name_of_student st1;;


type teacher = {tname: string; office: string; ext: int};;
type student_teacher = {s: student; t: teacher};;
let x = {s = st1; t = {tname = "Kazuya Numata"; office = "150"; ext = 12345}};;

(* Variant *)
type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int;;

let c = Circle 3;;
let figs = [Point; Squire 5; Rectangle (4,5); Circle 3];;

let area_of_figure = function
Point -> 0
  | Circle r -> r * r * 3
  | Rectangle (x, y) -> x * y
  | Square x -> x * x;;

area_of_figure c;;

let similar x y =
  match (x, y) with
      (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
    | (Rectangle (l1, l2), Rectangle (l3, l4)) when l1 * l4 - l2 * l3 =0 -> true
    | _ -> false;;

similar (Rectangle (2,4)) (Rectangle (1,2));;

type color = Black | Blue | Red | Magenta | Green | Cyan | Yellow | White;;

let reverse_color = function
Black -> White
  | Blue -> Yellow
  | Red -> Cyan
  | Magenta -> Green
  | Green -> Magenta
  | Cyan -> Red
  | Yellow -> Blue
  | White -> Black;;

reverse_color White;;


(* 再起Variant *)
type nat = Zero | OneMoreThan of nat;;

let zero = Zero
and two = OneMoreThan (OneMoreThan Zero);;


let rec add m n =
  match m with
      Zero -> n
    | OneMoreThan m' -> OneMoreThan (add m' n);;

add two two;;
add two zero;;
(*
  add two zero
  -> add OneMoreThan(OneMoreThan Zero) Zero
  -> OneMoreThan(add (OneMoreThan Zero) Zero)
  -> OneMoreThan(OneMoreThan(add Zero Zero))
  -> OneMoreThan(OneMoreThan Zero)
*)

type intlist = INil | ICons of int * intlist;;

type even = Zero | OMT_E of odd
and odd = OMT_O of even;;

let rec o_plus_e (OMT_O e1) e2 = OMT_O (e_plus_e e1 e2)
and e_plus_e e1 e2 =
  match e1 with
      Zero -> e2
    | OMT_E o -> OMT_E (o_plus_e o e2);;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

type 'a with_location = {loc_x: float; loc_y: float; body: 'a};;
{loc_x = 0.0; loc_y = 0.0; body = Point};;

type ('a, 'b) list_with_tail =
    Nil of 'b | Cons of 'a * ('a, 'b) list_with_tail;;

Cons ( 2, Cons(3, Nil("end")));;

(* option *)
let fact n =
  let rec fact' n =
    if n = 0 then 1 else n * fact' (n-1) in
  if n < 0 then None else Some (fact' n);;

fact 3;;
fact (-10);;


type ('a, 'b) sum = Left of 'a | Right of 'b;;


type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
let chartree = Br ('a', Br ('b', Br ('d', Lf, Lf), Lf),
		   Br ('c', Br ('e', Lf, Lf), Br('f', Lf, Lf)));;

let rec size = function
  Lf -> 0
  | Br (_, left, right) -> 1 + size left + size right;;

let rec depth = function
  Lf -> 0
  | Br (_, left, right) -> 1 + max (depth left) (depth right);;


let comptree = Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)),
                  Br(3, Br(6, Lf, Lf),
                     Br(7, Lf, Lf)));;

size comptree;;
depth comptree;;

let rec preorder = function
  Lf -> []
  | Br (x, left, right) -> x::(preorder left) @ (preorder right);;

preorder comptree;;

let rec inorder = function
  Lf -> []
  | Br (x, left, right) -> (inorder left) @ (x::inorder right);;

inorder comptree;;

let rec postorder = function
  Lf -> []
  | Br (x, left, right) -> (postorder left) @ (postorder right) @ [x];;
postorder comptree;;


let rec preord t l =
  match t with
      Lf -> l
    | Br(x, left, right) -> x::(preord left (preord right l));;

preord comptree [];;

(* rose tree *)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;

let addressbook =
  XBr ("addressbook", [
    XBr ("person", [
      XBr ("name", [XLf (Some "Kazuya NUMATA")]);
      XBr ("tel", [XLf (Some "011-111-2222")])]);
    XBr("person", [XLf None]);
    XBr("person", [XLf None])]);;

let rec string_of_xml = function
XBr (tag, xml_list) -> "<" ^ tag ^ ">"
  ^ string_of_xmllist xml_list
  ^ "</" ^ tag ^">"
  | XLf None -> ""
  | XLf (Some s) -> s
and string_of_xmllist = function
[] -> ""
  | xml :: rest -> string_of_xml xml ^ string_of_xmllist rest;;

string_of_xml addressbook;;

let rec rosetree_of_tree = function
Lf -> RLf
  | Br (a, left, right) -> RBr(a, map rosetree_of_tree [left; right]);;

let rec tree_of_rtree = function
RLf -> Br (None, Lf, Lf)
  | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
[] -> Lf
  | rtree :: rest -> let Br (a, left, Lf) = tree_of_rtree rtree in
		     Br (a, left, tree_of_rtreelist rest);;
(*  rtree 部分が b以下 、restがe, f以下になり、元々はaの子だったが、Br (a, left, tree_of_rtreelist rest)の部分で、e,fがbの子要素になる *)

let rtree =
  RBr ("a", [
    RBr ("b", [
      RBr ("c", [RLf]);
      RLf;
      RBr ("d", [RLf])]);
    RBr ("e", [RLf]);
    RBr ("f", [RLf])]);;

tree_of_rtree rtree;;

type intseq = Cons of int * (int -> intseq);;

let rec f x = Cons(x+1, f);;
f 0;;
f 1;;
f 3;;

let rec step2 x = Cons(x+2, step2);;

let Cons(x1, f1) = step2 0
let Cons(x2, f2) = f1 x1
let Cons(x3, f3) = f2 x2;;

let rec step n x = Cons(x+n, step (n+1));;
let Cons(x1, f1) = step 1 0;;
let Cons(x2, f2) = f1 x1;;
let Cons(x3, f3) = f2 x2;;
let Cons(x4, f4) = f3 x3;;
let Cons(x5, f5) = f4 x4;;

let rec nthseq n (Cons(x, f)) =
  if n = 1 then x
  else nthseq (n-1) (f x);;

nthseq 7 (step2 0);;
nthseq 6 (step 1 0);;

let is_prime x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
  in not (is_divisible_from_2_to (x - 1));;

is_prime 3;;
is_prime 9;;
is_prime 97;;

let rec next_prime x =
  if is_prime (x + 1) then x + 1 else next_prime(x + 1);;

next_prime 7;;
next_prime 20;;
next_prime 97;;

let rec prime_seq x =
  if is_prime(x+1) then Cons(x+1, prime_seq) else prime_seq (x+1);;
nthseq 20 (prime_seq 1);;

