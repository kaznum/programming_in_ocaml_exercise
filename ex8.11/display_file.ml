let display_file fname =
  let fd = open_in fname in
  let output_each () =
    while true do
      print_string (input_line fd ^ "\n");
    done in
  try output_each () with End_of_file -> ();
  close_in fd;;

display_file "sample.txt";;
