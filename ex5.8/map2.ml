let rec nth n l =
  match (n, l) with
      (1, a :: _) -> a
    | (n', _ :: rest) when n' > 0 -> nth (n-1) rest;;

let rec fold_left f e l =
  match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest;;

let map2 f xs =
  let results = fold_left (fun x y -> (f y)::x) [] xs in
  fold_left (fun x y -> y::x) [] results;;

map2 (fun x -> ( * ) 2 x) [1;2;3;4];;
