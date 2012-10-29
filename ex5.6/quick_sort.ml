let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed
  in t -. m *. floor ( t/. m);;
let rec randlist n seed tail =
  if n = 0 then (seed, tail)
  else randlist (n - 1) (nextrand seed) (seed::tail);;

let rec quick_sort = function
([] | [_]) as l -> l
  | pivot :: rest ->
    let rec partition left right = function
    [] ->
      (quick_sort left) @ (pivot :: quick_sort right)
      | y::ys ->
        if pivot < y then partition left ( y::right) ys
        else partition (y :: left) right ys
    in partition [] [] rest;;

let rec quick_sort2 results = function
    [] -> results
  | [x] -> x::results
  | pivot :: rest ->
    let rec partition left right results = function
       [] -> let right_results = pivot :: quick_sort2 results right in
	  quick_sort2 right_results left
      | y::ys ->
        if pivot < y then partition left ( y::right) results ys
        else partition (y :: left) right results ys
    in partition [] [] results rest;;

quick_sort ( snd (randlist 100000 1.0 []));;
quick_sort2 [] ( snd (randlist 100000 1.0 []));;
