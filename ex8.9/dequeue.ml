type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;
type 'a queue = { mutable head : 'a mlist; mutable tail : 'a mlist };;

let create () = { head = MNil; tail = MNil };;

let add a = function
    { head = MNil; tail = MNil } as q ->
      let c = MCons (a, ref MNil) in
      q.head <- c; q.tail <- c
  | {tail = MCons(_, next)} as q ->
    let c = MCons (a, ref MNil) in
    next := c; q.tail <- c
  | _ -> failwith "enqueue: input queue broken";;


let peek = function
    {head = MNil; tail = MNil} -> failwith "hd: queue is empty"
  | {head = MCons(a, _)} -> a
  | _ -> failwith "hd: queue is broken";;

peek q;;

(* for ex8.9 *)
let take = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q -> q.head <- !next; if !next = MNil then q.tail <- MNil; a
  | _ -> failwith "dequeue: queue is broken";;

let q : int queue = create ();;
add 1 q; add 2 q; add 3 q;;
q;;
take q;;
q;;
take q;;
q;;
add 4 q; take q;;
q;;

take q;;
q;;

ignore(take q); add 5 q; peek q;;
q;;
