let dollar_of_yen yen =
  let rate = 114.32 in
  floor(float_of_int(yen) /. rate *. 100.0 +. 0.5) /. 100.0;;

