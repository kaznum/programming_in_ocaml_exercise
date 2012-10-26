let trapezoid_area = fun h a b ->
  (a +. b) *. h /. 2.0;;

let rec integral = fun f a b ->
  let dx = 0.1e-3 in
  if b < a then 0.0
  else
    let next = a +. dx in
    trapezoid_area dx (f a) (f next) +. integral f next b;;

integral sin 0.0 3.14159262;;
