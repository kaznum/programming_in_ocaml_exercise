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

type teacher = { name : string; mutable office : string };;

let t = { name = "Igarashi"; office = "140" };;

t.office <- "142";;
t;;

t.office.[2] <- '3';;
t;;

let p = ref 5 and q = ref 2;;

(!p, !q);;
(p, q);;

p := !p + !q;;
(!p, !q);;

let reflist = [p;q;p];;
p := 100;;
reflist;;

let p = ref 5 and q = ref 2;;
let refp = ref p and refq = ref q;;
!refq := !(!refp);;
(!p, !q);;

p := 5;;
p;;
!p;;
let p = ref 6;;
p;;
!p;;

let ar = [|1;2|];;
let [|b;c|] = ar;;

let x = ref [];;

(* 本ではうまく動作するように記載されているが、実際にはエラーになる  under ocaml 4.0 *)
(2 :: !x, true :: !x);;

x := [1];;

let (get, set) =
  let r = ref [] in
  ((fun () -> !r), (fun x -> r:=x));;


1 :: get ();;
"abc" :: get ();; (* この時点でエラーになる *)
set ["abc"];; (* これもエラーになる *)
1 :: get ();; (* [1]が戻り値になる *)

(* 8.3 *)
let () = print_string "Hello, " in
print_string "World!\n";;

(* 引数の評価順序は不定(実装依存) *)
let f x y = 2 in
f (print_string "Hello, ") (print_string "World\n");;
(print_string "Hello, ", print_string "World\n");;

ignore;;

let print_hello () = print_string "Hello, "; 0;;
print_hello (); print_string "World";;

ignore (print_hello ()); print_string "World";;

let f1 b = if b then print_string "a"; print_string "b\n";;
let f2 b = (if b then print_string "a"); print_string "b\n";;
f1 false;;
f2 false;;

if true then begin print_string "a"; print_string "b" end;;

let fact n =
  let i = ref 1 and res = ref 1 in
  while (!i <= n) do
    res := !res * !i; i := !i+1
  done;
  !res;;

fact 5;;

let parrot () =
  let s = ref "" in
  while (s := read_line (); !s <> ".") do
    print_string !s;
    print_endline !s
  done;;

parrot ();;

let rec whle condition body =
  if condition () then
    begin body (); whle condition body end;;

let fact n =
  let i = ref 1 and res = ref 1 in
  whle (fun () -> (!i <= n))
    (fun () -> i := !i+1; res := !res * !i);
  !res;;

fact 5;;


let rec iter f = function
  [] -> ()
  | a :: rest -> begin f a; iter f rest end;;

iter (fun s -> print_string "Station "; print_endline s)
  ["Tokyo"; "Sapporo"; "Naogya"];;

(* 8.4 *)
type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;
type 'a queue = { mutable head : 'a mlist; mutable tail : 'a mlist };;

let create () = { head = MNil; tail = MNil };;

let q : int queue = create ();;

let add a = function
    { head = MNil; tail = MNil } as q ->
      let c = MCons (a, ref MNil) in
      q.head <- c; q.tail <- c
  | {tail = MCons(_, next)} as q ->
    let c = MCons (a, ref MNil) in
    next := c; q.tail <- c
  | _ -> failwith "enqueue: input queue broken";;

add 1 q; add 2 q; add 3 q;;
q;;

let peek = function
    {head = MNil; tail = MNil} -> failwith "hd: queue is empty"
  | {head = MCons(a, _)} -> a
  | _ -> failwith "hd: queue is broken";;

peek q;;

let take = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q -> q.head <- !next; a
  | _ -> failwith "dequeue: queue is broken";;

take q;;
q;;
take q;;
q;;
add 4 q; take q;;
q;;

ignore(take q); add 5 q; peek q;;
q;;

