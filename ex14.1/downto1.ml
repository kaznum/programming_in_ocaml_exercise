let rec downto1 x =
  match x with
      0 -> `Nil
    | x' -> `Cons(x, downto1 (x' - 1));;

downto1 5;;
