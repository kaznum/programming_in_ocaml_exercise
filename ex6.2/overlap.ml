type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int;;

type 'a with_location = {loc_x: float; loc_y: float; body: 'a};;


let overlap_point_point f1 f2 =
  (f1.loc_x = f2.loc_x) && (f1.loc_y = f2.loc_y);;
let overlap_point_circle f1 f2 =
  let Circle r = f2.body in
  (f2.loc_x -. f1.loc_x) ** 2.0 +. (f2.loc_y -. f1.loc_y) ** 2.0 <= float_of_int (r * r);;

let overlap_point_rectangle f1 f2 =
    let {loc_x = loc_x; loc_y = loc_y; body = Rectangle (x, y)} = f2 in
    (loc_x -. float_of_int(x) /. 2.0 <= f1.loc_x) &&
      (loc_x +. float_of_int(x) /. 2.0 >= f1.loc_x) &&
      (loc_y -. float_of_int(y) /. 2.0 <= f1.loc_y) &&
      (loc_y +. float_of_int(y) /. 2.0 >= f1.loc_y);;

let overlap_circle_circle f1 f2 =
  match (f1.body, f2.body) with
      (Circle r1, Circle r2) ->
	(f1.loc_x -. f2.loc_x) ** 2.0 +. (f1.loc_y -. f2.loc_y) ** 2.0 <= float_of_int (2 * (r1 + r2))
    | (_, _) -> false;;

let corners_of_rectangle f =
  let {loc_x = rx; loc_y = ry; body = Rectangle (x, y)} = f in
  ((rx -. (float_of_int x) /. 2.0, ry -. (float_of_int y) /. 2.0),
   (rx -. (float_of_int x) /. 2.0, ry +. (float_of_int y) /. 2.0),
   (rx +. (float_of_int x) /. 2.0, ry -. (float_of_int y) /. 2.0),
   (rx +. (float_of_int x) /. 2.0, ry +. (float_of_int y) /. 2.0));;

let in_circle point cir =
  let px = fst point in
  let py = snd point in
  let { loc_x = cx; loc_y = cy; body = Circle r} = cir in
  (cx -. px) ** 2.0 +. (cy -. py) ** 2.0 <= float_of_int(r * r);;

let in_rectangle point rect =
  let { loc_x = lx; loc_y = ly; body = Rectangle(x,y) } = rect in
  (fst point) >= (lx -. (float_of_int x) /. 2.0) &&
    (fst point) <= (lx +. (float_of_int x) /. 2.0) &&
    (snd point) >= (ly -. (float_of_int y) /. 2.0) &&
    (snd point) <= (ly +. (float_of_int y) /. 2.0);;

let overlap_circle_rectangle f1 f2 =
  let {loc_x = cx; loc_y = cy; body = Circle r} = f1 in
  let (c1, c2, c3, c4) = corners_of_rectangle f2 in
  in_circle c1 f1 || in_circle c2 f1 || in_circle c3 f1 || in_circle c4 f1 ||
    in_rectangle (cx -. float_of_int r, cy) f2 ||
    in_rectangle (cx +. float_of_int r, cy) f2 ||
    in_rectangle (cx, cy -. float_of_int r) f2 ||
    in_rectangle (cx, cy +. float_of_int r) f2;;


let overlap_rectangle_rectangle f1 f2 =
  let {loc_x = rx1; loc_y = ry1; body = Rectangle (x1, y1)} = f1 in
  let {loc_x = rx2; loc_y = ry2; body = Rectangle (x2, y2)} = f2 in
  if x1 * y1 < x2 * y2 then
    let (c1, c2, c3, c4) = corners_of_rectangle f1 in
    in_rectangle c1 f2 || in_rectangle c2 f2 || in_rectangle c3 f2 || in_rectangle c4 f2
  else
    let (c1, c2, c3, c4) = corners_of_rectangle f2 in
    in_rectangle c1 f1 || in_rectangle c2 f1 || in_rectangle c3 f1 || in_rectangle c4 f1;;

let rectangle_of_square f =
  match f.body with
      Square x' ->
	{f with body = Rectangle (x',x')}
    | _ ->
      f;;

let overlap f1 f2 =
  match (f1.body) with
      Point ->
	(match f2.body with
	    Point ->
	      overlap_point_point f1 f2
	  | Circle r ->
	    overlap_point_circle f1 f2
	  | Square x ->
	    overlap_point_rectangle f1 (rectangle_of_square f2)
	  | Rectangle (x, y) ->
	    overlap_point_rectangle f1 f2
	)
    | Circle r ->
      (match f2.body with
	  Point ->
	    overlap_point_circle f2 f1
	| Circle r' ->
	  overlap_circle_circle f1 f2
	| Rectangle(x, y) ->
	  overlap_circle_rectangle f1 f2
	| Square x ->
	  overlap_circle_rectangle f1 (rectangle_of_square f2)
      )
    | Rectangle (_, _) | Square _ ->
      (match f2.body with
	  Point ->
	    overlap_point_rectangle f2 (rectangle_of_square f1)
	| Circle r' ->
	  overlap_circle_rectangle f2 (rectangle_of_square f1)
	| Rectangle (x', y') ->
	  overlap_rectangle_rectangle (rectangle_of_square f1) (rectangle_of_square f2)
	| Square x' ->
	  overlap_rectangle_rectangle (rectangle_of_square f1) (rectangle_of_square f2)
      )
;;

(* Point - Point *)
(* false *)
 overlap {loc_x = 3.0; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Point};;

(* true *)
overlap {loc_x = 3.0; loc_y = 1.0; body = Point} {loc_x = 3.0; loc_y = 1.0; body = Point};;

(* Point - Circle *)
(* false *)
overlap {loc_x = 3.0; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Circle 1};;

(* true *)
overlap {loc_x = 0.5; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Circle 1};;

(* Point - Rectangle *)
(* false *)
overlap {loc_x = 10.5; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Square 2};;
(* true *)
overlap {loc_x = 0.5; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Rectangle (2, 2)};;

(* Point - Square *)
(* false *)
overlap {loc_x = 10.0; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Square 2};;
(* true *)
overlap {loc_x = 0.5; loc_y = 1.0; body = Point} {loc_x = 0.0; loc_y = 1.0; body = Square 2};;

(* Circle - Circle *)
(* false *)
overlap {loc_x = 3.0; loc_y = 1.0; body = Circle 1} {loc_x = 0.0; loc_y = 1.0; body = Circle 1};;
(* true *)
overlap {loc_x = 1.5; loc_y = 1.0; body = Circle 1} {loc_x = 0.0; loc_y = 1.0; body = Circle 1};;

(* Circle - Rectangle *)
(* false *)
overlap {loc_x = 1.5; loc_y = 1.0; body = Circle 1} {loc_x = 10.0; loc_y = 1.0; body = Rectangle (3,5)};;
(* true *)
overlap {loc_x = 1.5; loc_y = 1.0; body = Circle 1} {loc_x = 0.0; loc_y = 1.0; body = Rectangle (3,5)};;

(* Rectangle - Rectangle *)
(* false *)
overlap {loc_x = 0.0; loc_y = 2.0; body = Rectangle (1,2)} {loc_x = 10.0; loc_y = 1.0; body = Rectangle (1,2)};;

(* true *)
overlap {loc_x = 0.0; loc_y = 2.0; body = Rectangle (3,5)} {loc_x = 0.0; loc_y = 1.0; body = Rectangle (3,5)};;

(* Rectangle - Square *)
(* false *)
overlap {loc_x = 0.0; loc_y = 2.0; body = Rectangle (1,2)} {loc_x = 10.0; loc_y = 1.0; body = Square 2};;

(* true *)
overlap {loc_x = 0.0; loc_y = 2.0; body = Rectangle (3,5)} {loc_x = 0.0; loc_y = 1.0; body = Square 5};;

