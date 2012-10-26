let id x = x;;
let ($) f g x = f (g x);;

let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f;;

funny (( + ) 2) 3 4;;
funny (( + ) 2) 4 6;;
funny (( * ) 2) 3 4;;

(*
funny (( + ) 2) 3 4;;
  -> funny (f $ f) (3 / 2) $ f 4
  -> funny (f $ f) 1 $ f 4
  -> funny (f $ f) 1 6
  -> funny ((f $ f) $ (f $ f)) (1/2) $ (f $ f) 6
  -> funny ((f $ f) $ (f $ f)) 0 10
  -> 10

funny (( + ) 2) 4 6;;
  -> funny (f $ f) (4 / 2) 6
  -> funny (f $ f) 2 6
  -> funny ((f $ f) $ (f $ f)) (2 / 2) 6
  -> funny ((f $ f) $ (f $ f)) 1 6
  -> funny (((f $ f) $ (f $ f)) $ ((f $ f) $ (f $ f))) (1 / 2) ((f $ f) $ (f $ f)) 6
  -> funny (((f $ f) $ (f $ f)) $ ((f $ f) $ (f $ f))) 0 14
  -> 14

funny (( * ) 2) 3 4;;
  -> funny (f $ f) (3 / 2) $ f 4
  -> funny (f $ f) 1 8
  -> funny ((f $ f) $ (f $ f)) (1/2) $ (f $ f) 8
  -> funny ((f $ f) $ (f $ f)) 0 32
  -> 32

nが偶数でも奇数でも、fを引数に対してn回適用する
*)
