let rec concat = function
[] -> []
  | []::rest -> concat rest
  | (x::xs)::rest -> x::(concat (xs::rest));;

concat [[0; 3; 4]; [2]; []; [5; 0]];;
