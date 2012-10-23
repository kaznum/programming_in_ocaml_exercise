let yen_string_of_dollar dollar = 
  let yen_of_dollar dollar =
    let rate = 114.32 in
      int_of_float ( rate *. dollar ) in
  string_of_float(dollar) ^ " dollars are " ^ string_of_int(yen_of_dollar dollar) ^ " yen.";;
  
