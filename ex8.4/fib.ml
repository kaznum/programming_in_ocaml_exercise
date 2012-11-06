let fib n =
  let prev = ref 0 and curr = ref 1 and nxt = ref 1 in
  for x = 1 to n do
    nxt := !prev + !curr;
    prev := !curr;
    curr := !nxt
  done;
  !curr;;

fib 10;;

let fib' n =
  let i = ref 0 and prev = ref 0 and curr = ref 1 and nxt = ref 1 in
  while (!i < n) do
    nxt := !prev + !curr;
    prev := !curr;
    curr := !nxt;
    i := !i + 1;
  done;
  !curr;;

fib' 10;;
