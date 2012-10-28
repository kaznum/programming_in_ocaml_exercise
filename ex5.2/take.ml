let rec take n list =
  match (n, list) with
      (0, _) | (_, []) -> []
    | (n, x::xs) -> x::(take (n-1) xs);;
	
let ten_to_zero = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1];;
take 8 ten_to_zero;;
