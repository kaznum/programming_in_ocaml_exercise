module IntSet = Set.Make (
  struct
    type t = int
    let compare i j = i - j
  end);;

(* 以下は、Makeの引数の内容を一度moduleで定義してからIntSetを定義 *)
(*
module IntElm =
  struct
    type t = int
    let compare i j = i - j
  end

module IntSet = Set.Make (IntElm);;
*)

open IntSet;;


let s1 = add 2 (add 1 empty)
and s2 = add 1 (add 3 empty);;
(mem 1 s1, mem 2 s1, mem 3 s1);;
let s3 = inter s1 s2 in
(mem 1 s3, mem 2 s3, mem 3 s3);;


module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end;;

module MakeSet ( Order : OrderedType ) =
  struct
    type elt = Order.t
    type t = elt list

    let empty = []

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

    let rec elements s = s
  end;;

module MyIntSet = MakeSet(struct
  type t = int
  let compare i j = i - j
end);;

open MyIntSet;;
let s1 = add 2 (add 1 empty)
and s2 = add 1 (add 3 empty);;
(mem 1 s1, mem 2 s1, mem 3 s1);;
let s3 = inter s1 s2 in
(mem 1 s3, mem 2 s3, mem 3 s3);;


(* tを隠蔽 *)
module MakeAbstractSet (Order : OrderedType) :
  sig
    type elt = Order.t
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end
  =
  struct
    type elt = Order.t
    type t = elt list

    let empty = []

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

    let rec elements s = s
  end;;

module AbstractIntSet = MakeAbstractSet (
  struct
    type t = int
    let compare i j = i - j
  end
);;

open AbstractIntSet;;
let s1 = add 2 (add 1 empty)
let s2 = add 1 (add 3 empty);;

(mem 1 s1, mem 2 s1, mem 3 s1);;
let s3 = inter s1 s2 in
(mem 1 s3, mem 2 s3, mem 3 s3);;

(* sigを抽出 *)
module type SET =
  sig
    type elt
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end;;

module MakeAbstractSet' (Order : OrderedType)
  : SET with type elt = Order.t
  =
  struct
    type elt = Order.t
    type t = elt list

    let empty = []

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

    let rec elements s = s
  end;;

module AbstractIntSet' = MakeAbstractSet' (
  struct
    type t = int
    let compare i j = i - j
  end
);;

open AbstractIntSet';;
let s1 = add 2 (add 1 empty)
let s2 = add 1 (add 3 empty);;

(mem 1 s1, mem 2 s1, mem 3 s1);;
let s3 = inter s1 s2 in
(mem 1 s3, mem 2 s3, mem 3 s3);;


(* 複数の引数をとるファンクター*)
module Pair =
  struct
    module Elt =
      struct
	type t = int
	let compare i j = i - j
      end
    module Set = MakeAbstractSet'(Elt)
  end;;

module type Psig =
  sig
    module Elt : OrderedType
    module Set : SET with type elt = Elt.t
  end;;

module MakeTest(P : Psig) =
  struct
    let test_elements set =
      let rec loop = function
          [] | [_] -> true
	| x::y::rest ->
	  if P.Elt.compare x y > 0 then false
	  else loop (y::rest)
      in loop (P.Set.elements set)
  end;;

module Test = MakeTest (Pair);;
Test.test_elements (Pair.Set.add 1 (Pair.Set.add 2 Pair.Set.empty));;
