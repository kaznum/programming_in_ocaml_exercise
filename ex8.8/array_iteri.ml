let rec array_iteri f a =
  let index = ref 0 in
  try
    while true do
      f !index a.(!index);
      index := !index + 1;
    done
  with Invalid_argument "index out of bounds" -> ();;

array_iteri (fun i s -> print_string "Station #"; print_int i; print_string": "; print_endline s; ) [|"Tokyo"; "Sapporo"|];;
