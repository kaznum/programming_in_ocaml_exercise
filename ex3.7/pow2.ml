let rec pow (x, n) =
  if n = 0 then
    1.0
  else
    let res = pow (x, n / 2) in
    if n mod 2 = 0 then
      res *. res
    else
      res *. res *. x;;
