let diff s1 s2 =
  let commons = intersect s1 s2 in
  let rec append_unless_exist s1 commons =
    match
      s1
    with
	[] -> []
      | x::xs when mem x commons -> append_unless_exist xs commons
      | x::xs -> x::(append_unless_exist xs commons) in
  append_unless_exist s1 commons;;

diff [2;1;3;4;5] [1;3;7];;
