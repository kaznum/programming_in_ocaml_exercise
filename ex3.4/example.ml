let x = 1 in let x = 3 in let x = x + 2 in x * x;;
(*
# let x = 1 in let x = 3 in let x = x + 2 in x * x;;
Characters 4-5:
  let x = 1 in let x = 3 in let x = x + 2 in x * x;;
      ^
Warning 26: unused variable x.
- : int = 25
*)
let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y;;
(*
# let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y;;
- : int = 13
*)

let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z;;
(*
# let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z;;
Characters 17-18:
  let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z;;
                   ^
Warning 26: unused variable y.
- : int = 16
*)

