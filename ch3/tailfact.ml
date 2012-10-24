let rec tailfact (n, res) =
  if n = 1 then res
  else tailfact (n - 1, n * res);;

let rec fact n =
  if n = 1 then 1
  else n * fact (n-1);;

(* tailfact は n * resを先に計算してから再帰呼び出しするので、スタックが積み上がらないため、リソースに優しい *)
