(*
  It is easy with String.sub

  let len = String.length str in
  if len = 1 then str.[0]
  else
    let remaining = String.sub str 1 (len - 1) in
    let current = max_ascii(remaining) in
    if str.[0] < current then current else str.[0];;

*)
let max_ascii (str : string) =
  let len = String.length str in
  let rec compare (x, index) =
    if index >= len then x
    else
      if x < str.[index] then compare(str.[index], index + 1)
      else compare(x, index + 1) in
  compare (str.[0], 1);;
