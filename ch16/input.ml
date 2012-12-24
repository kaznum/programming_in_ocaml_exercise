open Str

let rec input_spec ch =
  let s = try input_line ch with End_of_file -> "--" in
  if string_match (regexp "^;") s 0 then input_spec ch
  else if string_match (regexp "^--") s 0 then []
  else
    let spec =
      List.map (int_of_string) (split (regexp "[ \t]+") s ) in
    spec :: input_spec ch

let input_board filename =
  let ch = open_in filename in
  let h_spec = input_spec ch in
  let v_spec = input_spec ch in
  Board.board_of_spec ~h_spec ~v_spec
