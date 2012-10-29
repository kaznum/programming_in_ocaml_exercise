let squares r =
  let max = int_of_float(sqrt (float_of_int r)) in
  let does_match x y =
    if (x * x) + (y * y) = r then true else false in
  let rec downto1 a =
    if a = 0 then [] else a:: downto1 (a - 1) in
  let rec exists f list =
    fold_right (fun x -> (||) (f x) ) list false in
  let rec count_matched xs =
    match xs with
	[] -> 0
      | x::xs' ->
	let f = does_match x in
	(if exists f (downto1 x) then 1 else 0) + (count_matched xs') in
  count_matched (downto1 max);;

squares 48612265;;
