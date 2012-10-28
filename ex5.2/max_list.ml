let max_list list =
  let rec max_and_list max list =
    match list with
	[] -> max
      | x::xs when x > max -> max_and_list x xs
      | _::xs -> max_and_list max xs in
  match list with
      x::xs -> max_and_list x xs;;
	
max_list [7; 9; 0; -5];;
