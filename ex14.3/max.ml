let rec max = function
    `Cons(x, `Nil) -> x
  | `Cons(x, (`Cons(_, _) as l)) ->
    let m =
      max l in if x > m then x else m;;

let make_max f = function
    `Nil -> 0
  | `Cons (a, l) ->
    if a < (f l) then f l else a;;

let make_amax f = function
    (`Nil | `Cons (_,_)) as l -> make_max f l
  | `App (l1, l2) ->
    let first = f l1 and second = f l2 in
    if first < second then second else first;;

let rec amax l = make_amax amax l;;

(* test *)
let l1 = `Nil
and l2 = `Cons(1, `Nil)
and l3 = `Cons(2, `Cons(1, `Nil));;

let l4 = `App (l2, l3);;
let l5 = `App(l4, `Cons(5, `Cons(2, `Nil)));;

amax l1;;
amax l2;;
amax l3;;
amax l4;;
amax l5;;
