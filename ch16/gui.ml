open Tk
open MySupport
open Board


let c_height = 1
let c_width = 2

let defcol = `White
let pushcol = `Blue
let selcol = `Color "#ffdfdf"

let color_of_state = function
    Pressed -> pushcol
  | NotPressed -> defcol

let relief_of_state = function
    Pressed -> `Sunken
  | NotPressed -> `Ridge

let toggle = function
    Pressed -> NotPressed
  | NotPressed -> Pressed

let rec string_of_spec sep = function
    [] -> ""
  | [i] -> string_of_int i
  | i::rest -> (string_of_int i) ^ sep ^ string_of_spec sep rest


(* 操作 *)
let focus label _ = Label.configure label ~background:selcol
let unfocus label st _ =
  Label.configure label ~background:(color_of_state !st)

let pressed label st _ =
  st := toggle !st;
  Label.configure label ~relief:(relief_of_state !st) ~background:(color_of_state !st)

(* 1マスをクリア *)
let clear label st _ =
  st := NotPressed;
  Label.configure label
    ~relief:(relief_of_state !st)
    ~background:(color_of_state NotPressed)

(* 全マスをクリア *)
let clear_all cells states =
  List.iter2
    (fun (_::c_row) st_row ->
      List.iter2 (fun c_row st_row -> clear c_row st_row ()) c_row st_row) cells states

(* quit *)
let quit () = closeTk(); exit 0

(* 正誤をlabelに表示 *)
let check h_spec v_spec body label () =
  if is_solved h_spec v_spec body
  then Label.configure label ~text:"正解!" ~foreground:`Red
  else Label.configure label ~text:"残念..." ~foreground:`Blue

