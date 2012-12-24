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


(* widget作成 *)

(* １行を作成 *)
let rec make_cells ?(width=c_width) ?(height=c_height) parent = function
    [] -> []
  | c::rest ->
    let label =
      Label.create parent ~width ~height ~relief:`Ridge
	~background:(color_of_state !c) in
    bind ~events:[`Enter] ~action:(focus label) label;
    bind ~events:[`Leave] ~action:(unfocus label c) label;
    bind ~events:[`ButtonPress] ~action:(pressed label c) label;
    label :: make_cells ~width ~height parent rest

(* 縦方向のラベルのリスト *)
let make_vspec ?(spwidth=c_width) ~spheight speclist parent =
  List.map
    (fun s ->
      Label.create parent ~width:spwidth ~height:spheight ~text:s ~anchor:`S ~relief:`Groove)
    (List.map (string_of_spec "\n") speclist)

(* 横方向のラベルとマス目のリスト *)
let make_row ~spwidth ?(height=c_height) ?(width=c_width) spec parent cell_list =
  let s = string_of_spec " " spec in
  Label.create parent ~width:spwidth ~height ~text:s ~anchor:`E ~relief:`Groove :: (make_cells ~height ~width parent cell_list)

let make_board { width=width;
		 height=height;
		 h_spec=h_spec;
		 v_spec=v_spec;
		 body=body} b_clear b_check parent =
  (* 幅と高さの計算 *)
  let spwidth = max (max_list (List.map List.length h_spec) * 2) 10 in
  let spheight = max (max_list (List.map List.length v_spec)) 4 in

  let f1 = Frame.create parent in
  let corner = Label.create ~width:spwidth ~height:spheight f1 in
  let reset_corner _ =
    Label.configure corner
      ~relief:`Raised ~text:"お絵かき\nロジック" ~foreground:`Black in
  reset_corner ();

  bind ~events:[`ButtonPress] ~action:reset_corner corner;
  Button.configure b_check ~command:(check h_spec v_spec body corner);

  let row0 = corner::make_vspec v_spec ~spheight f1 in
  pack row0 ~side:`Left ~anchor:`S;

  let frame_rows = make_list height (fun () -> Frame.create parent) in
  pack (f1 :: frame_rows) ~side:`Top;

  let rows = map3 (make_row ~spwidth) h_spec frame_rows body in
  List.iter (pack ~side:`Left) rows;
  Button.configure b_clear ~command:(fun () -> clear_all rows body)

(* main *)

let () =
  if Array.length Sys.argv = 1 then failwith "Usage: ilogic filename"
  else
    begin
      let top = openTk() in

      let fr_board = Frame.create top in
      let fr_buttons = Frame.create top in
      pack [fr_board; fr_buttons] ~side:`Left ~fill:`Y;

      let b_check = Button.create ~text:"解答チェック" fr_buttons in
      let b_clear = Button.create ~text:"やり直し" fr_buttons in
      let b_quit = Button.create ~text:"終了" ~command:quit  fr_buttons in
      pack [b_check; b_clear; b_quit] ~side:`Top ~fill:`X;

      let board = Input.input_board (Sys.argv.(1)) in
      make_board board b_clear b_check fr_board;
      Wm.title_set top "お絵かきロジック";
      mainLoop()
    end
