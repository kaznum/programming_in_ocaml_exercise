let f = (+) 1;;
let g = ( * ) 2;;
let l = [1;2;3;4;5];;
map f (map g l);;

map (fun x -> f (g x)) l;;

