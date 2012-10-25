(*
fib 4
-> let (i, _) = fib_pair 4 in i
-> let (i, _) = (if 4 = 1 then (1,0) else let (i, j) = fib_pair(4-1) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = fib_pair(3) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = fib_pair((if 3 = 1 then (1,0) else let (i, j) = fib_pair(3-1) in (i + j, i))) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = (let (i, j) = fib_pair(2) in (i + j, i)) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = (let (i, j) = ((if 2 = 1 then (1,0) else let (i, j) = fib_pair(2-1) in (i + j, i))) in (i + j, i)) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = (let (i, j) = ((let (i, j) = fib_pair(1) in (i + j, i))) in (i + j, i)) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = (let (i, j) = ((let (i, j) = (1,0) in (i + j, i))) in (i + j, i)) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = (let (i, j) = (1, 1) in (i + j, i)) in (i + j, i)) in i
-> let (i, _) = (let (i, j) = (2, 1) in (i + j, i)) in i
-> let (i, _) = (3, 2) in i
-> 3
*)
let fib n =
  let rec fib_pair n =
    if n = 1 then (1, 0)
    else
      let (i, j) = fib_pair (n - 1) in
      (i + j, i)
  in
  let (i, _) = fib_pair n in i;;
