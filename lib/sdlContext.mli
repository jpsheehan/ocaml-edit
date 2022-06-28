type t
type font
type texture

val create : unit -> t
val destroy : t -> unit
val present : t -> unit
val clear : t -> unit
val set_target : t -> texture option -> unit
val set_title : t -> string -> unit
val set_draw_color : t -> Color.t -> unit
val set_draw_blend_mode : t -> Blend.t -> unit
val delay : t -> int -> unit
val copy : t -> texture -> src:Rect.t -> dst:Rect.t -> unit
val fill_rect : t -> Rect.t option -> unit
val draw_line : t -> int -> int -> int -> int -> unit
val get_rect : t -> Rect.t

(* until we can put these in some sort of "submodule" (would we want to?) just pop these here*)
val font_create : string -> int -> font
val font_get_width_of_text : font -> string -> int

val font_create_texture_from_text :
  font -> t -> Color.t -> Color.t -> string -> texture * Rect.t

val font_height : font -> int
val font_size_utf8 : font -> string -> int * int

(* Same with the texture stuff *)

val texture_create : w:int -> h:int -> texture
val texture_destroy : texture -> unit