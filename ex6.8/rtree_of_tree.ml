(* tree *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* rose tree *)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

let rec tree_of_rtree = function
RLf -> Br (None, Lf, Lf)
  | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
[] -> Lf
  | rtree :: rest -> let Br (a, left, Lf) = tree_of_rtree rtree in
                     Br (a, left, tree_of_rtreelist rest);;
let map f list =
  match list with
      [] -> []
    | x::xs ->
      (f x)::(map f xs);;

let rec brothers t =
  match t with
      Lf -> []
    | Br(x, left, right) ->
      (Br(x, left, Lf)::brothers(right));;

let rec rtree_of_tree t =
  match t with
      Br(Some x, left, _) ->
        RBr(x, (map rtree_of_tree (brothers left)))
    | Br(None, _, _) -> RLf
    | Lf -> raise (Sys_error "Lf exists");;


let rtree =
  RBr ("a",
       [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]);
	RBr ("e", [RLf]);
	RBr ("f", [RLf])
       ]);;

let tree =
  Br (Some "a",
      Br (Some "b",
	  Br (Some "c", Br (None, Lf, Lf),
	      Br (None, Lf, Br (Some "d", Br (None, Lf, Lf), Lf))),
	  Br (Some "e", Br (None, Lf, Lf), Br (Some "f", Br (None, Lf, Lf), Lf))),
      Lf);;

rtree_of_tree(tree_of_rtree rtree) = rtree ;;
tree_of_rtree(rtree_of_tree tree) = tree;;
rtree_of_tree tree = rtree;;
tree_of_rtree rtree = tree;;
