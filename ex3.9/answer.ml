(*
fact 4
-> cond((4 = 1), 1, 4 * fact (4-1))
-> cond(false, 1, 4 * fact 3)
-> if false then 1 else 4 * fact 3
-> if false then 1 else 4 * ( cond((3 = 1), 1, 3 * fact (3-1)) )
-> if false then 1 else 4 * ( cond(false, 1, 3 * fact 2) )
-> if false then 1 else 4 * ( cond(false, 1, 3 * ( cond((2 = 1), 1, 2 * fact (2-1)) )))
-> if false then 1 else 4 * ( cond(false, 1, 3 * ( cond(false, 1, 2 * fact 1))))
-> if false then 1 else 4 * ( cond(false, 1, 3 * ( cond(false, 1, 2 * (cond((1 = 1), 1, 1 * fact (1-1)))))))
-> if false then 1 else 4 * ( cond(false, 1, 3 * ( cond(false, 1, 2 * (cond(true, 1, 1 * fact 0))))))
-> if false then 1 else 4 * ( cond(false, 1, 3 * ( cond(false, 1, 2 * (cond(true, 1, 1 * ( cond((0 = 1), 1, 0 * fact (0-1)) )))))))
-> if false then 1 else 4 * ( cond(false, 1, 3 * ( cond(false, 1, 2 * (cond(true, 1, 1 * ( cond(false, 1, 0 * fact (-1)) )))))))

  <= 値渡しであるため、fact 0を評価しようとして、Stackが溢れる
*)

let cond (b, e1, e2) : int = if b then e1 else e2;;
let rec fact n = cond((n = 1), 1, n * fact (n-1));;
