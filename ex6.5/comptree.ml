type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x n =
  match n with
      0 -> Lf
    | _ -> Br (x, comptree x (n-1), comptree x(n-1));;

comptree "a" 3;;
(*
          1
       3     2
     7   6 5   4
*)
let rec comptree' n =
  let rec comptree_with_root n' r =
    match n' with
	0 -> Lf
      | _ -> Br (r, comptree_with_root (n'-1) (r*2+1), comptree_with_root (n'-1) (r*2)) in
  comptree_with_root n 1;;
	  
comptree' 4;;
