let rec pow  = fun n x ->
  if n = 0 then
    1.0
  else
    let res = pow (n / 2) x in
    if n mod 2 = 0 then
      res *. res
    else
      res *. res *. x;;

let cube = pow 3;;

let rec pow2  = fun x n ->
  if n = 0 then
    1.0
  else
    let res = pow (n / 2) x in
    if n mod 2 = 0 then
      res *. res
    else
      res *. res *. x;;

let cube2 x = pow2 x 3;;
