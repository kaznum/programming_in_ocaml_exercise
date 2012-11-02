raise Division_by_zero;;
raise Not_found;;
raise (Sys_error "File not found");;
raise (Invalid_argument "fact: negative argument");;

let rec fact n =
  if n < 0 then raise (Invalid_argument "fact: negative argument")
  else if n = 0 then 1 else n * fact (n-1);;

10 + fact (-4);;

let rec find x = function
[] -> raise Not_found
  | a :: l when a = x -> 1
  | _ :: l -> 1 + find x l;;

find 7 [0;8;7;3];;
find 9 [0;8;7;3];;

try find 7 [0;8;7;3] with Not_found -> 0;;
try find 9 [0;8;7;3] with Not_found -> 0;;

let rec find' x = function
[] -> raise Not_found
  | a :: l when a = x -> 1
  | _ :: l -> 1 + find x l
let find x l = try find' x l with Not_found -> 0;;


find 7 [0;8;7;3];;
find 9 [];;
find 9 [0;8;7;3];;

let rec map f list =
  match list with
      [] -> []
    | x::rest -> (f x)::(map f rest);;

let map_sqrt l =
  let sqrt' x = if x < 0.0 then raise (Invalid_argument "sqrt'")
    else sqrt x
  in try Some (map sqrt' l) with Invalid_argument "sqrt'" -> None;;

map_sqrt [1.0; 2.0; 0.25];;
map_sqrt [1.0; -2.0; 0.25];;

try find 9 [0;8;7;3/0] with
    Not_found -> 0
  | Division_by_zero -> -1;;

try find' 9 [0;8;7;3] with Division_by_zero -> 0;;

try find' 9 [0;8;7;3] with
    Not_found -> 1/0
  | Division_by_zero -> -1;;
