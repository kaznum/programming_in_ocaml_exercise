let display_linenum = ref false
let display_wordnum = ref false
let display_bytenum = ref false
let filenames = ref []

let spec = [("-l", Arg.Set display_linenum, "Display the number of lines");
	    ("-w", Arg.Set display_wordnum, "Display the number of words");
	    ("-c", Arg.Set display_bytenum, "Display the number of bytes")]


let read_and_calc file =
  let lines = ref 0 in
  let words = ref 0 in
  let bytes = ref 0 in
  let in_word = ref false in
  let ch = open_in file in
  try
    while true do
      let c = input_char ch in
      bytes := !bytes + 1;
      match (Char.escaped c) with
	  "\\n" ->
	    if !in_word then begin words := !words + 1; in_word := false end;
	    lines := !lines + 1
        | "\\t" | " " ->
	  if !in_word then begin words := !words + 1; in_word := false; end
	| _ ->
	  in_word := true
    done;
    (0, 0, 0)
  with End_of_file ->
    close_in ch;
    (!bytes, !words, !lines)


let display_results b w l file =
  let show_all = ref true in
  let (bytes, words, lines) = read_and_calc file in
  if b or w or l then show_all := false;
  if b or !show_all then Printf.printf "%d byte(s) " bytes;
  if w or !show_all then Printf.printf "%d word(s) " words;
  if l or !show_all then Printf.printf "%d line(s) " lines;
  Printf.printf " %s\n" file

let _ =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: wc [-c] [-w] [-l] [-help] filename ...";
  List.iter (fun s -> display_results !display_bytenum !display_wordnum !display_linenum s) (List.rev !filenames)

