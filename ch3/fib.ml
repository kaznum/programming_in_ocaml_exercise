let rec fib n =
  let rec fib_pair n =
    if n = 1 then (1, 0)
    else
      let (i, j) = fib_pair (n - 1) in
      (i + j, i)
  in
  let (i, _) = fib_pair n in i;;
