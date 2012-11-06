let ref' x = { contents = x};;

let (!!) x = x.contents;;

let (+:) x y =
  x.contents <- y;;

let x = ref 3;;
x;;
!!x;;

let w = ref 3;;
w +: 4;;
w;;
!w;;
