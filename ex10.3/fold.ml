let width = ref 80
let filenames = ref []

let spec = [("--width", Arg.Int (fun i -> width := i), "Characters in a line")]


let read_and_display file =
  let bytes = ref 0 in
  let ch = open_in file in
  try
    while true do
      let c = input_char ch in
      bytes := !bytes + 1;
      (match (Char.escaped c) with
	  "\\n" ->
	    bytes := 0
	| _ ->
	  if !bytes >= !width then
	    begin
	      print_string "\n";
	      bytes := 0
	    end
      );
      print_char c;
    done;
  with End_of_file -> close_in ch


let _ =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: fold [--width num] [-help] filename ...";
  List.iter (fun s -> read_and_display s) (List.rev !filenames)

