class calc =
object
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method plus = let x = num in func <- (fun y -> x + y)
  method eq = func num
end
;;
			      
let c = new calc;;
c#input 4; c#plus; c#input 2; c#eq;;

type base =
    Oct | Dec | Hex;;

class calc_base b =
let string_of = function
    Oct -> Printf.sprintf "%o"
  | Dec -> Printf.sprintf "%d"
  | Hex -> Printf.sprintf "%x"
in
object
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method plus = let x = num in func <- (fun y -> x + y)
  method eq = string_of b (func num)
end
;;

let c = new calc_base Hex;;

c#input 17; c#plus; c#input 30; c#eq;;


(* the following raise error *)
calc_base Oct;;

new calc_base;;

class calc_double =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method plus = let x = num in func <- (fun y -> x + y)
  method eq = func num
  method double = self#plus; self#eq
end;;

let d = new calc_double in
d#input 13; d#double;;

class calc_many_buttons =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x

  method shift n = num <- num * 10 + n
  method zero = self#shift 0
  method one = self#shift 1
  method two = self#shift 2
  method three = self#shift 3
  method four = self#shift 4
  method five = self#shift 5
  method six = self#shift 6
  method seven = self#shift 7
  method eight = self#shift 8
  method nine = self#shift 9

  method plus = let x = num in func <- (fun y -> x + y);
		num <- 0

  method eq = let r = func num in
	      num <- 0;
	      func <- (fun n -> n);
		r
end;;

let c = new calc_many_buttons;;
c#one; c#zero; c#plus; c#two; c#nine; c#eq;;
c#one; c#shift 32; c#plus; c#shift 11; c#eq;;

(* make method 'shift' private *)
class calc_many_buttons' =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x

  method private shift n = num <- num * 10 + n
  method zero = self#shift 0
  method one = self#shift 1
  method two = self#shift 2
  method three = self#shift 3
  method four = self#shift 4
  method five = self#shift 5
  method six = self#shift 6
  method seven = self#shift 7
  method eight = self#shift 8
  method nine = self#shift 9

  method plus = let x = num in func <- (fun y -> x + y);
		num <- 0

  method eq = let r = func num in
	      num <- 0;
	      func <- (fun n -> n);
		r
end;;
let c = new calc_many_buttons';;
c#one; c#zero; c#plus; c#two; c#nine; c#eq;;
c#one; c#shift 32; c#plus; c#shift 11; c#eq;;


(* initializer *)

class demo_calc n m =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method plus = let x = num in func <- (fun y -> x + y)
  method eq = func num

  initializer
    self#input n; self#plus; self#input m;
    Printf.printf "%d + %d = %d\n" n m (self#eq)
end
;;

new demo_calc 32 28;;
