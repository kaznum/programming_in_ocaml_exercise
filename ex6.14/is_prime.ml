(* original (110.212s)
let is_prime primes x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
  in not (is_divisible_from_2_to (x - 1));;
*)

(* 6.14-1 (16.086s)
let is_prime primes num =
  let rec is_divisible_by n =
    (n < num) && ((num mod n = 0) || is_divisible_by (n + 1))
  in not (is_divisible_by (2));;
*)

(* 6.14-2 (2.180s) *)
let is_prime primes x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
  in not (is_divisible_from_2_to (int_of_float(floor(sqrt(float_of_int(x))))));;

(* 6.14-3 (2.013s)
let is_prime primes x =
  match primes with
      [] -> true
    | p::rest when p >= x -> true
    | p::rest -> if p < x && x mod p = 0 then false else is_prime rest x;;
*)


(* 6.14-4  (4.653s) *)
let rec is_prime primes x =
  let max = int_of_float(floor(sqrt(float_of_int(x)))) in
  match primes with
      [] -> true
    | p::rest when p >= x -> true
    | p::rest -> if (p <= max) && (x mod p = 0) then false else is_prime rest x;;


let rec prime_seq primes x =
  if is_prime primes (x+1) then Cons(x+1, prime_seq (primes @ [x+1])) else prime_seq primes (x+1);;

let a = Sys.time () in
ignore( nthseq 10000 (prime_seq [] 1));
print_float(Sys.time() -. a);;

