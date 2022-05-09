type document

val create_empty : Tsdl_ttf.Ttf.font -> document
val create_from_file : Tsdl_ttf.Ttf.font -> string -> document
val create_from_string : Tsdl_ttf.Ttf.font -> string -> document
val process_hook : document -> int -> Tsdl.Sdl.rect -> document

val render_hook :
  document -> Tsdl.Sdl.renderer -> Tsdl_ttf.Ttf.font -> Tsdl.Sdl.texture option

val prerender_hook :
  document ->
  Tsdl.Sdl.renderer ->
  Helpers.point ->
  Helpers.size ->
  Tsdl.Sdl.Pixel.format_enum ->
  document

val postrender_hook : document -> document
val event_hook : document -> Tsdl.Sdl.event -> document