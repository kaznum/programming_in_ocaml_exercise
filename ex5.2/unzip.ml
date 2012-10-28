let rec unzip ps =
  match ps with
      [] -> ([], [])
    | (x, y)::ps -> (x::(fst (unzip ps)), y::(snd (unzip ps)));;


unzip(zip[2;3;4;5;6;7;8;9;10;11]
        [true; true; false; true; false; true; false; false; false; true]);;

zip[2;3;4;5;6;7;8;9;10;11] [true; true; false; true; false; true; false; false; false; true];;

unzip [(2, true); (3, true); (4, false); (5, true); (6, false); (7, true);
 (8, false); (9, false); (10, false); (11, true)];;
