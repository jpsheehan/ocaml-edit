type t

val create :
  string -> int -> Tsdl.Sdl.color -> Tsdl.Sdl.color -> Tsdl.Sdl.color -> t

val get_fg_color : t -> Tsdl.Sdl.color
val get_bg_color : t -> Tsdl.Sdl.color
val get_selection_color : t -> Tsdl.Sdl.color
val get_text_font : t -> Tsdl_ttf.Ttf.font