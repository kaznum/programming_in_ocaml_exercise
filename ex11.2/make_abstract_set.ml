module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end;;

(* sigを抽出 *)
module type SET =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val union : t -> t -> t
    val diff : t -> t -> t
    val elements : t -> elt list
  end;;

module MakeAbstractSet (Order : OrderedType)
  : SET with type elt = Order.t
  =
  struct
    type elt = Order.t
    type t = elt list

    let empty = []

    let is_empty = function
        [] -> true
      | _ -> false

    let rec mem elt = function
        [] -> false
      | x :: rest ->
        let r = Order.compare elt x in
        (r = 0) || ((r < 0) && mem elt rest)

    let rec add elt = function
        [] -> [elt]
      | (x :: rest as s) ->
        match Order.compare elt x with
            0 -> s
          | r when r < 0 -> elt :: s
          | _ -> x :: (add elt rest)

    let rec inter s1 s2 =
      match (s1, s2) with
          (s1, []) -> []
        | ([], s2) -> []
        | ((e1::rest1 as s1), (e2::rest2 as s2)) ->
           match Order.compare e1 e2 with
               0 -> e1 :: inter rest1 rest2
             | r when r < 0 -> inter rest1 s2
             | _ -> inter s1 rest2

    let rec union s1 s2 =
      match (s1, s2) with
	  (s1, []) -> s1
	| ([], s2) -> s2
        | ((e1::rest1 as s1), (e2::rest2 as s2)) ->
          match Order.compare e1 e2 with
               0 -> e1 :: union rest1 rest2
             | r when r <= 0 -> e1 :: union rest1 s2
             | _ -> e2 :: union s1 rest2

    let rec diff s1 s2 =
      match (s1, s2) with
	  (s1, []) -> s1
	| ([], s2) -> s2
        | ((e1::rest1 as s1), (e2::rest2 as s2)) ->
          match Order.compare e1 e2 with
               0 -> diff rest1 rest2
             | r when r < 0 -> e1 :: diff rest1 s2
             | _ -> e2 :: diff s1 rest2


    let rec elements s = s
  end;;

module AbstractIntSet = MakeAbstractSet (
  struct
    type t = int
    let compare i j = i - j
  end
);;

open AbstractIntSet;;
(* empty/add/mem/inter *)
let s1 = add 2 (add 1 empty)
let s2 = add 1 (add 3 empty);;

(mem 1 s1, mem 2 s1, mem 3 s1);;
let s3 = inter s1 s2 in
(mem 1 s3, mem 2 s3, mem 3 s3);;

(* is_empty *)
let s4 = empty in
(is_empty s4);;
let s5 = add 1 empty in
is_empty s5;;

(* union *)
elements (union s1 s2);;

(* diff *)
elements (diff s1 s2);;
