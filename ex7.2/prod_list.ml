let rec prod_list l =
  let prod_list' l =
    match l with
	[] -> 1
      | x :: rest ->
	if x = 0 then raise (Sys_error "0 exists")
	else  x * prod_list rest in
  try prod_list' l with Sys_error "0 exists" -> 0 ;;


prod_list [1;2;3;4;5];;
prod_list [1;2;0;4;5];;
