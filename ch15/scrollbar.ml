(* 15.2.11 *)

open Tk
let top = openTk()

let i = ref 0 and lb = Listbox.create top
let b = Button.create top
  ~text: "Press me!"
  ~command: (fun () -> Listbox.insert ~index:`End ~texts:[string_of_int !i] lb; incr i)

let sb = Scrollbar.create top

let () =
  Listbox.configure ~yscrollcommand:(Scrollbar.set sb) lb;
  Scrollbar.configure ~command:(Listbox.yview lb) sb;
  pack [b];
  pack [coe lb; coe sb] ~side:`Left ~fill:`Y;
  mainLoop()
