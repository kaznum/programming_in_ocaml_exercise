type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;

let rec eval expression =
  match expression with
      Mul (a, b) -> eval a * eval b
    | Add (a, b) -> eval a + eval b
    | Const a -> a;;

eval exp = (3 + 4) * (2 + 5);;
