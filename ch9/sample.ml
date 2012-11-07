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

