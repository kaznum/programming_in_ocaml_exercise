let rec pow (x,n) =
  if n = 0 then 1.0 else x *. pow(x, n - 1);;
