let rec zip xs ys =
  match (xs, ys) with
      ([], []) -> []
    | (x::xs, y::ys) -> (x, y)::(zip xs ys);;

zip[2;3;4;5;6;7;8;9;10;11]
      [true; true; false; true; false; true; false; false; false; true];;

