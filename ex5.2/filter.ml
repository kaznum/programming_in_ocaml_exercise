let rec filter f list =
  match list with
      [] -> []
    | x::xs when f x -> x::(filter f xs)
    | x::xs -> (filter f xs);;

let is_positive x = (x > 0);;
filter is_positive [-9; 0; 2; 5; -3];;
filter (fun l -> List.length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]];;
