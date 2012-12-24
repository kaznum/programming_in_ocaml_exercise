(* 15.2.10 *)

open Tk
let top = openTk()

let i = ref 0 and lb = Listbox.create top
let b = Button.create top
  ~text: "Press me!"
  ~command: (fun () -> Listbox.insert ~index:`End ~texts:[string_of_int !i] lb; incr i)

let () =
  pack [coe lb; coe b] ~side:`Top;
  mainLoop()
