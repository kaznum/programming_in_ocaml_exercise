type ('a, 'b) t
val empty : ('a, 'b) t
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val retrieve : 'a -> ('a, 'b) t -> 'b option
val dump : ('a, 'b) t -> ('a * 'b) list

