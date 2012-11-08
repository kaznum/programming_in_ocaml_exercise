let ( <<< ) table (key, content) = Table.add key content table

let table = Table.empty
  <<< ("a", "the first letter of the English alphabet")
  <<< ("b", "the second letter of the English alphabet")
  <<< ("zzz", "sleeping noise")

let () =
  match Table.retrieve "a" table with
      Some x -> print_string x; print_newline ()
    | None -> ()


let table' = table <<< ("a", "an indefinite article")

let () =
  match Table.retrieve "a" table' with
      Some x -> print_string x; print_newline ()
    | None -> ()

let () =
  List.iter
    (fun (key, body) ->
      print_string key;
      print_string ": ";
      print_string body;
      print_newline())
    (Table.dump table')
