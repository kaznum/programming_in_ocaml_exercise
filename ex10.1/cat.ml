let ver = "0.1"
let display_linenum = ref false
let filenames = ref []

let spec = [("-n", Arg.Set display_linenum, "Display line number");
	    ("-version",
	     Arg.Unit
	       (fun () -> Printf.printf "cat in OCaml ver: %s\n" ver),
	     "Display version number")]


let read_and_display show_line_num file =
  let ch = open_in file in
  let n = ref 1 in
  try
    while true do
      if show_line_num then
	Printf.printf "%d: " !n;
      print_string ((input_line ch) ^ "\n");
      n := !n + 1
    done
  with End_of_file -> ();
  close_in ch

let _ =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] [-help] [-version] filename ...";
  List.iter (fun s -> read_and_display !display_linenum s) (List.rev !filenames);

