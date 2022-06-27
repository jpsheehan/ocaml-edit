type t

val create :
  string * int -> OEditor.Color.t -> OEditor.Color.t -> OEditor.Color.t -> t

val get_fg_color : t -> OEditor.Color.t
val get_bg_color : t -> OEditor.Color.t
val get_selection_color : t -> OEditor.Color.t
val get_text_font : t -> OEditor.SdlContext.font