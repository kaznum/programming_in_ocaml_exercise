let rec iterfact n =
  if n = 1 then 1 else n * iterfact (n - 1);;
