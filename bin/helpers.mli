val ( >>= ) : ('a, [< `Msg of string ]) result -> ('a -> 'b) -> 'b

type point = { x : int; y : int }
type size = { w : int; h : int }

val clamp : int -> int -> int -> int
val take : 'a list -> int -> 'a list
val skip : 'a list -> int -> 'a list
val replace : 'a list -> int -> 'a -> 'a list