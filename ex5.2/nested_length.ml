let rec nested_length = function
[] -> 0
  | []::xs -> nested_length(xs)
  | (x::rest)::lists ->
    1 + nested_length(rest::lists);;
  
nested_length [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;
