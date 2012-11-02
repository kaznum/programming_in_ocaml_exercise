type intseq = Cons of int * (int -> intseq);;

let rec nthseq n (Cons(x, f)) =
  if n = 1 then x
    else nthseq (n-1) (f x);;

let fib =
  let rec f m n = Cons(n, fun x -> f x (m+n)) in
  f 0 1;;

nthseq 1 fib;;
nthseq 2 fib;;
nthseq 3 fib;;
nthseq 4 fib;;
nthseq 5 fib;;
nthseq 6 fib;;
nthseq 7 fib;;
nthseq 10 fib;;

