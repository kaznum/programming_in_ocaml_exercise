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

(* inheritance *)
class calc_minus =
object
  inherit calc
  method minus = let x = num in func <- (fun y -> x - y)
end
;;
let cm = new calc_minus in
cm#input 13; cm#minus; cm#input 4; cm#eq;;

(* inherit and override *)
class calc_for_kids =
object
  inherit calc_minus
  method eq = max (func num) 0
end;;

let kc = new calc_for_kids in
kc#input 4; kc#minus; kc#input 13; kc#eq;;
let kc = new calc_for_kids in
kc#input 4; kc#minus; kc#input 3; kc#eq;;

(* inherit and call a super class method *)
class calc_for_kids2 =
  object
  inherit calc_minus as super
  method eq = max (super#eq) 0
end;;

let kc = new calc_for_kids2 in
kc#input 4; kc#minus; kc#input 13; kc#eq;;
let kc = new calc_for_kids2 in
kc#input 4; kc#minus; kc#input 3; kc#eq;;

(* annonymous class's object *)
let calc_obj =
object
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method plus = let x = num in func <- (fun y -> x + y)
  method eq = func num
end
;;

let foo x = if x then calc_obj else new calc;;
[calc_obj; new calc];;

(* 部分型, パラメトリック多相 *)

(* error *)
[new calc; new calc_double; new calc_minus];;

(* succeed *)
[new calc; (new calc_double :> calc); (new calc_minus :> calc)];;

let test_calc c = c#input 10; c#plus; c#input 20; c#eq = 30
in test_calc(new calc) && test_calc(new calc_for_kids);;

let test_calc c = c#input 10; c#plus; c#input 20; c#eq = 30;;
(*
  val test_calc : < eq : int; input : int -> 'a; plus : 'b; .. > -> bool =
  <fun>
*)

let input_ten (c : #calc) = c#input 10;;
input_ten (new calc_for_kids);;

(* error *)
input_ten (object method input i = print_int i end);;

let input_ten calc = calc#input 10
and push_plus calc = calc#plus;;

let f b = if b then input_ten else push_plus;;

let k1 a b = a
and k2 a b = b;;

let f b = if b then k1 else k2;;

let input_foo c = c#input "foo";;

(* error *)
let f b = if b then input_ten else input_foo;;

(* error *)
type t = <m: int; ..>;;
type pair = 'a * 'a ;;

(* virtual class/method *)
class virtual abstract_calc_demo n m op_name =
object (s)
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method virtual op : unit (* virtual *)
  method eq = func num
  initializer
    s#input n; s#op; s#input m;
    Printf.printf "%d %s %d = %d\n" n op_name m s#eq
end;;

(* error *)
new abstract_calc_demo 3 5 "op";;

class calc_demo_plus n m =
object
  inherit abstract_calc_demo n m "+"
  method op = let x = num in func <- (fun y -> x + y)
end;;
new calc_demo_plus 6 9;;

class calc_demo_mult n m =
object
  inherit abstract_calc_demo n m "*"
  method op = let x = num in func <- (fun y -> x * y)
end;;

new calc_demo_mult 6 9;;

class calc_many_buttons_00 =
object
  inherit calc_many_buttons' as super
  method zerozero = super#shift 0; super#shift 0
end;;

class calc_counter:
object
  inherit calc
  method get : int
end
  =
object (s)
  inherit calc as super
  val mutable c = 0

  method get = c
  method private incr = c <- c + 1
  method eq = s#incr; super#eq
end;;

class virtual foo :
object
  val a : int
  method virtual m : int -> int
end
    =
object
  val mutable a = 0
  method m x = 100 + x
end;;

class type calc_counter_t =
object
  inherit calc
  method get : int
end;;

class calc_counter : calc_counter_t =
object (s)
  inherit calc as super
  val mutable c = 0
  method get = c
  method private incr = c <- c+1
  method eq = s#incr; super#eq
end;;

