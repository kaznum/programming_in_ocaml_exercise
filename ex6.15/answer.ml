type ('a, 'b) sum = Left of 'a | Right of 'b;;

let f1 (a, b) =
  match b with
      Left x -> Left (a, x)
    | Right x -> Right (a, x);;


let f2 x =
  match x with
      Left (a, b) ->
	(a, Left b)
    | Right (a, b) ->
	(a, Right b);;


let f3 (x, y) =
  match (x, y) with
      (Left a, Left b) -> Left(Right(a, b))
    | (Right c, Right d) -> Right(Left(c, d));;

let f3 (x, y) =
  match (x, y) with
      (Left a, Left b) -> Left(Left(a, b))
    | (Right a, Right b) -> Right(Right(a, b))
    | (Left a, Right b) -> Right(Left(a, b))
    | (Right a, Left b) -> Left(Right(a, b));;

let f4 x =
  match x with
      Left(Left(a, b)) -> (Left a, Left b)
    | Right(Right(a, b)) -> (Right a, Right b)
    | Right(Left(a, b)) -> (Left a, Right b)
    | Left(Right(a, b)) -> (Right a, Left b);;

let f5 (f, g) x =
  match x with
      Left a -> f a
    | Right b -> g b;;

let f6 = fun x ->
  let f a = x (Left a) in
  let g b = x (Right b) in
  (f, g);;


let f7 f x =
  match f with
      Left a -> Left (a x)
    | Right b -> Right (b x);;

