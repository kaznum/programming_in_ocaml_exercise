(* 15.2.12 *)

open Tk
let top = openTk()

let sc = Scale.create top ~max:100. ~min:(-100.) ~showvalue:true
  ~tickinterval:20. ~label:"scale" ~length:400 ~orient:`Horizontal

let cb = Button.create top
  ~text:"Press me!"
  ~command:(fun () -> print_float (Scale.get sc); print_newline())

let () =
  pack [coe sc; coe cb];
  mainLoop()
