let rec list_of_alist alist =
  let rec val_list als =
    match als with
	`Nil -> []
      | `Cons (x, y) -> x :: val_list y
      | `App (x, y) -> val_list x @ val_list y in
  let rec list_of_vals vals =
    match vals with
	[] -> `Nil
      | x::xs -> `Cons(x, list_of_vals xs) in
  list_of_vals (val_list alist);;

list_of_alist (`Cons(0, `App (`Cons(1, `Cons(2,`Nil)), `Cons (3, `Cons(4, `Nil)))));;

