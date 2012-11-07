(* List *)
List.length [5;6;8];;

List.concat [[4;35;2]; [1]; [9; -4]];;

(* Queue *)
let q = Queue.create ();;
Queue.add 1 q; Queue.add 2 q;;
Queue.take q;;
Queue.take q;;
Queue.take q;;

(* Array *)
Array.make;;
Array.make 4 'a';;

Array.init;;
Array.init 9 (fun i -> char_of_int (i + 48));;
let arr = Array.init 9 (fun i -> char_of_int (i + 48));;
Array.length arr;;
Array.append arr (Array.init 9 (fun i -> char_of_int (i + 48)));;
Array.concat ((Array.make 3 'a')::(Array.make 3 'b')::[]);;
Array.map (fun i -> i * i) (Array.init 9 (fun i -> i));;
Array.iter (fun i -> print_string ((string_of_int i) ^ "\n")) (Array.init 9 (fun i -> i * i));;

let x = 110;;
Printf.printf "decimal: %d hexadecimal: %x string: %s\n" x x "foo";;
Printf.printf "pad with zero: %04d\n" 5;;

Printf.fprintf stdout "decimal: %d hexadecimal: %x string %s\n" x x "foo";;
Printf.sprintf  "decimal: %d hexadecimal: %x string %s\n" x x "foo";;

let f name age = name ^ (if age < 20 then ", you cannot vote." else ", you can vote.")
let g s = Scanf.sscanf s "%s is %d years old." f;;
g "Hogehoge is 20 years old.";;

g "I am 18 years old.";;

Printf.printf;;

"abc %s def %d";;
format_of_string "abc %s def %d";;

Printf.printf "abc %s def %d";;

#load "nums.cma";;
Num.add_num;;

open List;;

length [3;9;10];;

(* let x = Num.Int 1 and y = Num.Int 3;; *)
let x = Num.Int 1 and y = Num.Int 3;;
Num.(+/) x y;;

open Num;;
string_of_num (x //y +/ y // x);;

module Tree =
  struct
    type 'a t = Lf | Br of 'a * 'a t * 'a t

    let rec size = function
        Lf -> 0
      | Br (_, left, right) -> 1 + size left + size right

    let rec depth = function
        Lf -> 0
      | Br (_, left, right) -> 1 + max (depth left) (depth right)
  end;;


let tr = Tree.Br (1, Tree.Lf, Tree.Br(2, Tree.Lf, Tree.Lf));;
Tree.depth tr;;
Tree.size tr;;

module M =
  struct
    type r = {a: int; b: int}
  end;;

let x = {M.a = 1; M.b = 2};;

x.M.a + x.M.b;;

module Table =
  struct
    type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t
    let empty = Empty
    let add key datum table = Entry (key, datum, table)

    let rec retrieve key = function
        Empty -> None
      | Entry (key', datum, rest) ->
	if key = key' then Some datum else retrieve key rest

    let rec delete key = function
        Empty -> Empty
      | Entry (key', datum, rest) ->
	if key = key' then delete key rest
	else Entry (key', datum, delete key rest)

    let rec dump = function
        Empty -> []
      | Entry (key, contents, rest) ->
	(key, contents) :: (dump (delete key rest))
  end;;

let ( <<< ) table (key, content) = Table.add key content table;;

let table = Table.empty
  <<< ("a", "the first letter of the English alphabet")
  <<< ("b", "the second letter of the English alphabet")
  <<< ("zzz", "sleeping noise");;

Table.retrieve "a" table;;
let table' = table <<< ("a", "an indefinite article");;

Table.retrieve "a" table';;
table';;
Table.dump table';;

Table.delete "a" table';;

Table.Empty <<< ("z", "the last letter of the English alphabet");;

let is_empty = function
    Table.Empty -> true
  | Table.Entry _ -> false;;
is_empty table';;

module type TABLE1 =
  sig
    type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t
    val empty : ('a, 'b) t
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val retrieve : 'a -> ('a, 'b) t -> 'b option
    val dump : ('a, 'b) t -> ('a * 'b) list
  end;;

module Table1 : TABLE1 = Table;;

let ( <<< ) table (key, content) = Table1.add key content table in
let table = Table1.empty
  <<< ("a", "the first letter of the English alphabet")
  <<< ("b", "the second letter of the English alphabet")
  <<< ("zzz", "sleeping noise") in
let table' = table <<< ("a", "an indefinite article") in
(Table1.retrieve "a" table', Table1.dump table');;

Table1.delete "a" table';;
