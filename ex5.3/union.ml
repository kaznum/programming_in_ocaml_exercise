let rec union s1 s2 =
  let commons = intersect s1 s2 in
  let rec append_if_unique s1 commons =
    match s1 with
	[] -> s2
      | x::xs when mem x commons -> append_if_unique xs commons
      | x::xs -> x::(append_if_unique xs commons) in
  append_if_unique s1 commons;;
union [1;2;3;4;5] [1;3;5;6;7];;
