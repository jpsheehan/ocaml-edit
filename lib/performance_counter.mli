type 'a performance_counter

val create : int -> 'a performance_counter
val push : 'a performance_counter -> 'a -> 'a performance_counter
val clear : 'a performance_counter -> 'a performance_counter
val compute : 'a performance_counter -> ('a list -> 'b) -> 'b
val length : 'a performance_counter -> int
val nth : 'a performance_counter -> int -> 'a