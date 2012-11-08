type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t

let empty = Empty

let add key content table = Entry (key, content, table)

let rec retrieve key = function
    Empty -> None
  | Entry (key', content, rest) ->
    if key = key' then Some content else retrieve key rest

let rec delete key = function
    Empty -> Empty
  | Entry (key', content, rest) ->
    if key = key' then delete key rest
    else Entry (key', content, delete key rest)

let rec dump = function
    Empty -> []
  | Entry (key, content, rest) ->
    (key, content) :: (dump (delete key rest))
