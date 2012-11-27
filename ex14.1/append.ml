let rec append x y =
  match x with
      `Nil -> y
    | `Cons (x', y') -> `Cons (x', append y' y);;

append (`Cons(1, `Nil)) `Nil;;
append (`Cons(1, `Nil)) (`Cons(2, `Nil));;
append (`Cons(1, (`Cons(3, `Nil)))) (`Cons(2, `Nil));;
append (`Cons(1, (`Cons(3, `Nil)))) (`Cons(2, (`Cons(4, `Nil))));;
append `Nil `Nil;;
append `Nil (`Cons(1, `Nil));;
