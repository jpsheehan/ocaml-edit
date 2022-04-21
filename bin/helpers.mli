val ( >>= ) : ('a, [< `Msg of string ]) result -> ('a -> 'b) -> 'b

type point = { x : int; y : int }
type size = { w : int; h : int }
