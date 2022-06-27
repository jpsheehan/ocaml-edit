type t

val create : unit -> t
val create_from_string : string -> t
val create_from_file : string -> t
val create_from_docText : DocText.t -> t
val get_number_of_lines : t -> int
val get_line : t -> int -> string
val replace_line : t -> int -> string -> t
val remove_line : t -> int -> t
val insert_line_after : t -> int -> string -> t
val get_texture : t -> int -> (SdlContext.texture * Rect.t) option
val get_selection_texture : t -> int -> (SdlContext.texture * Rect.t) option
val flush_textures : t -> t

val prepare_textures :
  t ->
  SdlContext.t ->
  SdlContext.font ->
  Cursor.cursor ->
  Color.t ->
  Color.t ->
  int ->
  int ->
  t

val get_text : t -> DocText.t
val get_max_texture_width : t -> int
