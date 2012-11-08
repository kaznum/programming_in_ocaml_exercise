let _ =
  for i = 0 to Array.length Sys.argv - 1 do
    print_endline Sys.argv.(i)
  done;;


Sys.file_exists "sample.ml";;
Sys.remove;;

Sys.rename;;
Sys.getenv "HOME";;

Arg.parse;;
