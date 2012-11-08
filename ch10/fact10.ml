let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)
let () = print_int (fact 10); print_newline()
