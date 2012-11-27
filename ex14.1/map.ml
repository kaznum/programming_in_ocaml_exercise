let rec map f x =
  match x with
      `Nil -> `Nil
    | `Cons(a, b) -> `Cons(f a, map f b);;

map (( * ) 2) `Nil;;
map (( * ) 2) (`Cons(1, `Nil));;
map (( * ) 2) (`Cons(2, (`Cons(4, `Nil))));;

