type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;
type token = PCDATA of string | Open of string | Close of string;;

let rec children_tokens tokens level =
  match tokens with
    [] -> ([], [])
    | Close t :: rest when level = 1 -> (Close t :: [], rest)
    | Close t :: rest ->
      let (tokens', rest') = children_tokens rest (level - 1) in
      (Close t :: tokens', rest')
    | Open t :: rest ->
      let (tokens', rest') = children_tokens rest (level + 1) in
      (Open t :: tokens', rest')
    | PCDATA p :: rest ->
      let (tokens', rest') = children_tokens rest (level + 1) in
      (PCDATA p :: tokens', rest');;

let rec xml_of_tokens tokens =
  match tokens with
      [] -> []
    |      Open t :: rest ->
	let (tokens', rest') = children_tokens rest 0 in
	let children = xml_of_tokens(tokens') in
	if children = [] then
	  (XBr(t, [XLf None])) :: xml_of_tokens rest'
	else
          (XBr(t, children)) :: xml_of_tokens rest'
    | PCDATA p :: rest -> XLf (Some p) :: xml_of_tokens rest
    | Close t :: rest -> [];;

let token_list = [Open "a"; Open "b"; Open "b"; PCDATA "Hello1"; Close "b"; Close "b";
		  Open "c"; PCDATA "Hello"; Close "c"; Close "a"];;

#untrace children_tokens;;
children_tokens token_list 0;;

xml_of_tokens token_list;;
