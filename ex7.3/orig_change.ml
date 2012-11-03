let rec change coins amount = match (coins, amount) with
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
      if c > total then change rest total
      else c :: change coins (total - c);;

change [5; 2] 16;;
