let rec array_iter f a =
  let index = ref 0 in
  try
    while true do
      f a.(!index);
      index := !index + 1;
    done
  with Invalid_argument "index out of bounds" -> ();;

array_iter (fun s -> print_string "Station: "; print_endline s; ) [|"Tokyo"; "Sapporo"|];;
