let rec max_list = function
    [] -> 0
  | a::rest -> let m = max_list rest in if a > m then a else m

let rec make_list n f =
  if n = 0 then [] else f () :: make_list (n-1) f

let rec map3 f bs cs ds =
  match (bs, cs, ds) with
      ([],[],[]) -> []
    | (b::bs, c::cs, d::ds) -> f b c d :: map3 f bs cs ds
    | _ -> raise (Invalid_argument "map3")

let rec iter3 f bs cs ds =
  match (bs, cs, ds) with
      ([],[],[]) -> ()
    | (b::bs, c::cs, d::ds) -> f b c d; iter3 f bs cs ds
    | _ -> raise (Invalid_argument "iter3")

