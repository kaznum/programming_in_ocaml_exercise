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

module Queue2 : QUEUE =
struct
  type 'a t = Queue of ('a list * 'a list)
  let empty = Queue([], [])
  let peek = function Queue ([], _) -> raise Empty | Queue (x :: _, _) -> x
  let add q x =
    match q with
	Queue ([], _) -> Queue ([x], [])
      | Queue (xs, ys) -> Queue (xs, x::ys)
  let take q =
    match q with
	Queue ([], _) -> raise Empty
      | Queue (x::[], ys) ->
        (x, Queue ((List.fold_left (fun ts t -> t::ts ) [] ys), []))
      | Queue (x::xs, ys) -> (x, Queue (xs, ys))
  exception Empty
end
;;

let ( <<< ) q x = Queue2.add q x;;
let q = Queue2.empty <<< 1 <<< 5 <<< 4 <<< 3 <<< 2;;
Queue2.peek q;;
let (x, rest) = Queue2.take q;;
let (x, rest) = Queue2.take rest;;
let (x, rest) = Queue2.take rest;;
let (x, rest) = Queue2.take rest;;
let (x, rest) = Queue2.take rest;;
let (x, rest) = Queue2.take rest;;
