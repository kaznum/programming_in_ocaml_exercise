let x = ref [];;
(2 :: !x, true :: !x);; (* この時点でエラー *)

(*
# let x = ref [];;
val x : '_a list ref = {contents = []}
# (2 :: !x, true :: !x);;
Characters 18-20:
  (2 :: !x, true :: !x);;
                    ^^
Error: This expression has type int list
       but an expression was expected of type bool list

( 2:: !x )の時点で、x : int list ref になっているため、true :: !xを
しようとすると、型エラーになる。
*)
