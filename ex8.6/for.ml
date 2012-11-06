let rec for_to (init : int ref) max body =
  if !init <= max then
    begin body (); init := !init + 1; for_to init max body end;;

let rec for_downto (init : int ref) min body =
  if min <= !init then
    begin body (); init := !init - 1; for_downto init min body end;;


let n = ref 1 in
for_to n 10
  (fun () ->
    print_string ("the number is " ^ string_of_int(!n) ^ "\n");
  );;

let m = ref 10 in
for_downto m 1
  (fun () ->
    print_string ("the number is " ^ string_of_int(!m) ^ "\n");
  );;

