let rec repeat n str =
  match n with
      0 -> ""
    | _ -> str ^ repeat (n - 1) str;;
    
let rec roman pairs n =
  match (pairs, n) with
      ((_, 0) | ([],_)) -> ""
    | ((num, rom)::rest, _) ->
      (repeat ( n / num ) rom) ^ roman rest (n mod num);;

roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L");
         (10, "X"); (5, "V"); (1, "I")] 1984;;

roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD");
(100, "C"); (90, "XC"); (50, "L"); (40, "XL");
(10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984;;
