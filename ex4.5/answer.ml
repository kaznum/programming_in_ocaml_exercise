let twice f x = f (f x);;
(*
  twice twice f x
  -> twice (twice f) x ( by left join)
  -> (twice f (twice f x)) (by definition)
  -> twice f f(f x)
  -> f(f(f(f x))) (by definition)
*)

