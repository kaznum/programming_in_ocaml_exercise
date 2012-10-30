(* Variant *)
type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int;;

let c = Circle 3;;
let figs = [Point; Square 5; Rectangle (4,5); Circle 3];;

let area_of_figure = function
Point -> 0
  | Circle r -> r * r * 3
  | Rectangle (x, y) -> x * y
  | Square x -> x * x;;

area_of_figure c;;

let similar x y =
  match (x, y) with
      (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
    | (Rectangle (l1, l2), Rectangle (l3, l4)) when l1 * l4 - l2 * l3 =0 -> true
    | (Rectangle (l1, l2), Square _) when l1 = l2 -> true
    | (Square _, Rectangle (l1, l2)) when l1 = l2 -> true
    | _ -> false;;

similar (Rectangle (2,2)) (Square 3);;
