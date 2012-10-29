let rec intersect s1 s2 =
  match
    (s1, s2)
  with
      ([], _) -> []
    | (x::xs, ys) when mem x ys -> x::(intersect xs ys)
    | (x::xs, ys) -> intersect xs ys;;
intersect [1;3;5;7] [1;5;3;4;2];;
