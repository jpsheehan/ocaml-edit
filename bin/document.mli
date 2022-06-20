type document

val create_empty : Theme.t -> document
val create_from_file : Theme.t -> string -> document
val create_from_string : Theme.t -> string -> document
val process_hook : document -> int -> Tsdl.Sdl.rect -> document

val render_hook :
  document -> Tsdl.Sdl.renderer -> Theme.t -> Tsdl.Sdl.texture option

val prerender_hook :
  document ->
  Tsdl.Sdl.renderer ->
  OEditor.Helpers.point ->
  OEditor.Helpers.size ->
  Tsdl.Sdl.Pixel.format_enum ->
  document

val postrender_hook : document -> document
val event_hook : document -> Tsdl.Sdl.event -> document
val destroy : document -> unit
val get_changed : document -> bool
val set_changed : document -> bool -> document