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

let reverse l = fold_left (fun x y -> y::x) [] l;;
reverse [1;2;3;4;5];;

preorder(reflect(comptree)) = reverse(postorder(comptree));;
inorder(reflect(comptree)) = reverse(inorder(comptree));;
postorder(reflect(comptree)) = reverse(preorder(comptree));;
