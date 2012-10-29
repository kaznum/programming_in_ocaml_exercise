let rec mem a s =
    match s with
	[] -> false
      | x::xs when x = a -> true
      | _::xs -> mem a xs;;

mem 3 [1;2;3;4;5];;
mem 6 [1;2;3;4;5];;
