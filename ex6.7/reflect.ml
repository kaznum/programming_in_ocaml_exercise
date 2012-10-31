type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
let comptree = Br(1,
		  Br(2, Br(4, Lf, Lf),Br(5, Lf, Lf)),
                  Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)));;

let rec reflect t =
  match t with
      Lf -> Lf
    | Br(i, t1, t2) -> Br(i, reflect t2, reflect t1);;

reflect comptree;;

let rec preorder = function
  Lf -> []
  | Br (x, left, right) -> x::(preorder left) @ (preorder right);;

preorder comptree;;

let rec inorder = function
  Lf -> []
  | Br (x, left, right) -> (inorder left) @ (x::inorder right);;

inorder comptree;;

let rec postorder = function
  Lf -> []
  | Br (x, left, right) -> (postorder left) @ (postorder right) @ [x];;
postorder comptree;;

(* preorder(reflect(t)) *)
let rec preorder_reflect t =
  match t with
      Lf -> []
    | Br (x, left, right) -> x::(preorder_reflect right) @ (preorder_reflect left);;
preorder_reflect comptree;;
preorder(reflect(comptree));;

(* inorder(reflect(t)) *)
let rec inorder_reflect t =
  match t with
      Lf -> []
    | Br (x, left, right) -> (inorder_reflect right) @ x::(inorder_reflect left);;
inorder_reflect comptree;;
inorder(reflect(comptree));;

(* postorder(reflect(t)) *)
let rec postorder_reflect t =
  match t with
      Lf -> []
    | Br (x, left, right) -> (postorder_reflect right) @ (postorder_reflect left) @ [x];;
postorder_reflect comptree;;
postorder(reflect(comptree));;
