class calc =
object
  val mutable num = 0
  val mutable func = fun x -> x
  method input n = num <- n
  method plus =
    num <- func num;
    func <- (let x = num in fun y -> x + y)
  method eq = func num
end
;;

let c = new calc in
c#input 5; c#plus; c#input 8; c#plus; c#input 9; c#eq;;
