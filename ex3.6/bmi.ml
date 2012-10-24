let bmi (name, height, weight) =
  let value = weight /. (height *. height) in
  let result = if value < 18.5 then "やせています" else
      if value < 25.0 then "標準です" else
	if value < 30.0 then "肥満です" else "高度肥満です" in
  name ^ "さんは" ^ result;;
