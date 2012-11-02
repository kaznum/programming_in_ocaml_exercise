let rec find n list =
  let rec find' list =
    match list with
	[] -> raise Not_found
      | x::rest when x = n -> 1
      | _::rest -> 1 + find' rest
  in try Some (find' list) with Not_found -> None;;

find 7 [0;8;7;3];;
find 9 [0;8;7;3];;
