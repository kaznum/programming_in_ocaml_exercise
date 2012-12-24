(* 15.2.16 *)

open Tk
let top = openTk()

let cv = Canvas.create top
let c = Canvas.create_oval cv
  ~x1:10 ~y1:10 ~x2:100 ~y2:100 ~fill:`Blue ~width:5 ~outline:`Red

let b = Button.create top ~text:"Press me!"
  ~command:(fun () -> Canvas.configure_oval cv c ~fill:`Red)

let () =
  pack [coe b; coe cv];
  mainLoop()
