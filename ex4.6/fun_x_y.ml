let s x y z = x z (y z);;
let k x y = x;;

fun x y -> y;;

k (s k k) 1 3;;
