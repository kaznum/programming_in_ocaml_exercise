let rec drop n list =
  match list with
      [] -> []
    | _::xs when n > 0 -> drop (n-1) xs
    | xs when n = 0 -> xs;;

let ten_to_zero = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1];;
drop 7 ten_to_zero;;
