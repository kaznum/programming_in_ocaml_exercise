let cp fin fout  =
  let fdin = open_in fin in
  let fdout = open_out fout in
  let output_each () =
    while true do
      output_string fdout (input_line fdin ^ "\n");
    done in
  try output_each () with End_of_file -> ();
  close_out fdout;
  close_in fdin;;

cp "sample.txt" "output.txt";;
