module type QUEUE =
sig
  type 'a t
  val empty: 'a t
  val add: 'a t -> 'a -> 'a t
  val take: 'a t -> 'a * 'a t
  val peek: 'a t -> 'a
  exception Empty
end
;;

module Queue1 : QUEUE =
struct
  type 'a t = 'a list
  let empty = []
  let peek = function [] -> raise Empty | x :: rest -> x
  let add table x = table @ [x]
  let take table =
    match table with
	[] -> raise Empty
      | x :: rest -> (x, rest)
  exception Empty
end
;;

let ( <<< ) table x = Queue1.add table x;;
let table = Queue1.empty <<< 1 <<< 5 <<< 4 <<< 3 <<< 2;;
Queue1.peek table;;
let (q, rest) = Queue1.take table;;
let (q, rest) = Queue1.take rest;;
let (q, rest) = Queue1.take rest;;
let (q, rest) = Queue1.take rest;;
let (q, rest) = Queue1.take rest;;
let (q, rest) = Queue1.take rest;;
