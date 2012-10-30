type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
let comptree = Br(1,
		  Br(2, Br(4, Lf, Lf),Br(5, Lf, Lf)),
                  Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)));;
let rec postord t l =
  match t with
      Lf -> l
    | Br(x, left, right) -> postord left (postord right (x::l));;

postord comptree [];;
