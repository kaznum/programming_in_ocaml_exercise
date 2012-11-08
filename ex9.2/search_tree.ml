(* AbsTable *)
module type TABLE2 =
sig
  type ('a, 'b) t  (* abstracted *)
  val empty : ('a, 'b) t
  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val retrieve : 'a -> ('a, 'b) t -> 'b option
  val dump : ('a, 'b) t -> ('a * 'b) list
end


module SearchTree : TABLE2 =
struct
  type ('a, 'b) t = Lf | Br of ('a * 'b) * ('a, 'b) t * ('a, 'b) t

  let empty = Lf

  let rec add key value tree =
    match tree with
	Lf -> Br ((key, value), Lf, Lf)
      | (Br ((x, y), left, right) as whole) when x = key -> whole
      | Br ((x, y), left, right) when key < x -> Br((x, y), add key value left, right)
      | Br ((x, y), left, right) -> Br((x, y), left, add key value right);;

  let rec retrieve key tree =
    match tree with
	Lf -> None
      | Br ((x, y), left, right) ->
	if x = key then Some y
	else if key < x then retrieve key left else retrieve key right

  let rec dump tree =
    match tree with
	Lf -> []
      | Br((x, y), left, right) -> [(x, y)] @ (dump left) @ (dump right)
end;;

let ( <<< ) tree (key, value) = SearchTree.add key value tree;;
let tree = SearchTree.empty
  <<< (1, "a1")
  <<< (3, "a3")
  <<< (5, "a5")
  <<< (2, "a2")
  <<< (9, "a9")
  <<< (8, "a8")
  <<< (10, "a10")
  <<< (7, "a7")
  <<< (1, "xxx")
  <<< (6, "a6");;
SearchTree.dump tree;;
SearchTree.retrieve 1 tree;;
SearchTree.retrieve 9 tree;;
SearchTree.retrieve 15 tree;;
