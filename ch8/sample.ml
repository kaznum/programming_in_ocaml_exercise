print_string "Hello, World!\n";;
print_string "こんにちは\n";;

read_line ();;

();;

type 'a seq = Cons of 'a * (unit -> 'a seq);;

let rec from n = Cons (n, fun () -> from (n+1));;
let Cons(x1, f1) = from 1;;
let Cons(x2, f2) = f1 ();;
let Cons(x3, f3) = f2 ();;

let rec mapseq f (Cons (x, tail)) =
  Cons (f x, fun () -> mapseq f (tail ()));;

let reciprocals = mapseq (fun x -> 1.0 /. float_of_int x) (from 2);;

(* take 5 reciprocals;; *)


let s = "life";;

s.[2] <- 'k';;
s;;

let pair = ("life", 2);;
(fst pair).[2] <- 'k';;
pair;;


let pair = ("life", "life");;
(fst pair).[2] <- 'k';;
pair;;

let pair = let p = "life" in (p, p);;
(fst pair).[2] <- 'k';;
pair;;

let s = "life";;
let pair1 = ("life", s);;
let pair2 = (s, s);;

(pair1 = pair2, fst pair1 = fst pair2, snd pair1 = snd pair2);;
(pair1 == pair2, fst pair1 == fst pair2, snd pair1 == snd pair2);;


let update_string s1 s2 =
  let () = s1.[0] <- 'a' in
  let () = s2.[0] <- 'b' in
  s1.[0] = s2.[0];;

update_string "xyz" "xyz";;
let s = "xyz" in update_string s s;;

