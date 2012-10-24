(*
  the result of pos (x : int) is approximation of pi/4
  See Page 52
*)
let rec pos n =
  neg (n - 1) +. 1.0 /. (float_of_int (4 * n + 1))
and neg n =
  if n < 0 then 0.0
  else pos n -. 1.0 /. (float_of_int ( 4 * n + 3));;
