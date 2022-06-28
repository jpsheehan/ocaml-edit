type t

val create :
  string * int ->
  fg:Color.t ->
  bg:Color.t ->
  cursor:Color.t ->
  selection:Color.t ->
  t

val get_fg_color : t -> Color.t
val get_bg_color : t -> Color.t
val get_selection_color : t -> Color.t
val get_cursor_color : t -> Color.t
val get_text_font : t -> SdlContext.font