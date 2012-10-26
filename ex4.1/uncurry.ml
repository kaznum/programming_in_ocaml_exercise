let curry f x y = f (x, y);;
let average (x, y) = (x +. y) /. 2.0;;
let curried_avg = curry average;;

curried_avg 1.0 2.0;;

let uncurry f (x, y) = f x y;;

let avg = uncurry curried_avg in
avg (4.0, 5.3);;

let add = uncurry (+.) in
  add (3.1, 2.2);;
