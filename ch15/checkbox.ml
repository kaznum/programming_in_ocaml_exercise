open Tk

let top = openTk()

let tv = Textvariable.create ()

let cb = Checkbutton.create top
  ~text:"Press me!" ~variable:tv ~onvalue:"on" ~offvalue:"off"

let () =
  Checkbutton.configure cb
    ~command:
    (fun () -> Checkbutton.flash cb; print_endline (Textvariable.get tv));
  pack [cb] ~side:`Top;
  mainLoop()
