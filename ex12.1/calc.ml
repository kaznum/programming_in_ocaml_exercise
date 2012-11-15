(*
  method plus = func <- ( fun y -> num + y )
  としているが、numが2回目のinputで書き換えられるため、結局2回目に入力した数字の2倍の値が返される
  正解は以下のとおり
*)

class calc =
object
  val mutable num = 0
  val mutable func = fun x -> x
  method input n = num <- n
  method plus = func <- (let x = num in fun y -> x + y)
  method eq = func num
end
;;

let c = new calc in
c#input 5; c#plus; c#input 8; c#eq;;
