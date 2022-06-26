type t

val create : string -> int -> t
val get_width_of_text : t -> string -> int

val create_texture_from_text :
  t -> SdlContext.t -> Color.t -> Color.t -> string -> Texture.t * Rect.t
