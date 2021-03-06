val ( >>= ) : ('a, [< `Msg of string ]) result -> ('a -> 'b) -> 'b
(* val ( <| ) : 'a -> ('a -> 'b -> 'c) -> 'b -> 'c *)

type point = { x : int; y : int }
type size = { w : int; h : int }

val clamp : int -> int -> int -> int
val take : 'a list -> int -> 'a list
val skip : 'a list -> int -> 'a list
val replace : 'a list -> int -> 'a -> 'a list
val insert_after : 'a list -> int -> 'a -> 'a list
val remove : 'a list -> int -> 'a list
val split_string_at : string -> int -> string * string
val init : 'a list -> 'a list
val last : 'a list -> 'a option
val range : min:int -> max:int -> int list
val get_width_of_text : Tsdl_ttf.Ttf.font -> string -> int
val set_render_draw_color : Tsdl.Sdl.renderer -> Color.t -> unit Tsdl.Sdl.result
