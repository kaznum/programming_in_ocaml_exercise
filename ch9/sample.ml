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
