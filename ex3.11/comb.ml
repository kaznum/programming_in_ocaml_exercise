let rec comb (n, m) =
  if m = 0 || m = n then 1
  else
    comb(n - 1, m) + comb(n - 1, m - 1);;
