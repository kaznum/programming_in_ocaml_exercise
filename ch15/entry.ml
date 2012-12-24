(* 15.2.9 *)
open Tk
let top = openTk()

let cb = Entry.create top ~show:'*'

let b = Button.create top ~text: "Press Me!"
  ~command:(fun () -> print_endline (Entry.get cb))

let () =
  pack [coe cb; coe b] ~side:`Top;
  mainLoop()
