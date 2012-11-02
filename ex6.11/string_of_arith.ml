type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let rec string_of_arith expression =
  match expression with
      Mul (a, b) -> (string_of_arith a) ^ "*" ^ (string_of_arith b)
    | Add (a, b) -> "(" ^ (string_of_arith a) ^ "+" ^ (string_of_arith b) ^ ")"
    | Const a -> string_of_int a;;

string_of_arith exp;;

let rec expand expression =
  match expression with
    Mul (Add(a, b), x) -> Add(expand(Mul((expand a), expand(expand x))), expand(Mul((expand b), expand(expand x))))
    | Mul (x, Add(a, b)) -> Add(expand(Mul((expand x), expand(expand a))), expand(Mul((expand x), expand(expand b))))
    | Mul (x, y) -> Mul(expand x, expand y)
    | Add (a, b) -> Add(expand a, expand b)
    | Const _ as x -> x;;


let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;
expand exp;;
string_of_arith(expand exp);;

let exp2 = Mul (Add (Const 3, Const 4), Mul(Add(Const 6, Const 7), Add (Const 2, Const 5)));;
expand exp2;;
string_of_arith(expand exp2);;

let rec eval expression =
  match expression with
      Mul (a, b) -> eval a * eval b
    | Add (a, b) -> eval a + eval b
    | Const a -> a;;

eval exp2;;
