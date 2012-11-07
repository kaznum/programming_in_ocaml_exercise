let print_int' n =
  output_string stdout (string_of_int n);;

print_int' 5;;

(* following code is for lazy evaluation *)
let print_later n ()=
  output_string stdout (string_of_int n);;

let sample = print_later 5;;
sample ();;
