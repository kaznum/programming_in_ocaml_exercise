let x = ref 3;;

let incr x = x := !x + 1;;


incr x;;
!x;;
incr x;;
!x;;
