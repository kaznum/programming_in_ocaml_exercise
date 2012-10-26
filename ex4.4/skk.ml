let k x y = x;;
let s x y z = x z (y z);;

s k k 1;;
(*
  s k k 1
  -> k 1 (k 1)
  -> 1  ( because k x y = x )
*)
