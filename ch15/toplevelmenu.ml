(* 15.2.14 toplevel menu *)
open Tk
let top = openTk()

let menu = Menu.create top
let submenu = Menu.create menu

let () =
  Menu.add_command ~label:"Press Me!"
    ~command:(fun () -> print_string "Hello!"; print_newline()) menu;
  Menu.add_cascade ~label:"Cascade" ~menu:submenu menu;

  Menu.add_command ~label:"Don't Press Me!"
    ~command:(fun () -> closeTk(); exit 0) submenu;

  Menu.add_separator submenu;
  Menu.add_checkbutton ~label:"Check" submenu;

  Toplevel.configure ~menu:menu top;
  mainLoop ()
