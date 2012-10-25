let rec gcd (m, n) =
  if n mod m = 0 then m
  else
    if (n - m) < m then gcd(n - m, m) else gcd(m, n - m);;
