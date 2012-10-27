let rec downto1 n =
  if n = 0 then []
  else n :: downto1 (n-1);;
