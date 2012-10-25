let rec pos n =
  let neg n =
    if n < 0 then 0.0
    else pos n -. 1.0 /. (float_of_int ( 4 * n + 3)) in
  neg (n - 1) +. 1.0 /. (float_of_int (4 * n + 1));;
