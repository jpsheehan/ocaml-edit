type t

val create : unit -> t
val destroy : t -> unit
val present : t -> unit
val clear : t -> unit
val set_target : t -> Texture.t option -> unit
val set_title : t -> string -> unit
val set_draw_color : t -> Color.t -> unit
val delay : t -> int -> unit
val copy : t -> Texture.t -> src:Rect.t -> dst:Rect.t -> unit
val fill_rect : t -> Rect.t option -> unit
val get_rect : t -> Rect.t
val renderer : t -> Tsdl.Sdl.renderer