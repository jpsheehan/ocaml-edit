type document

val create_empty : OEditor.Theme.t -> document
val create_from_file : OEditor.Theme.t -> string -> document
val create_from_string : OEditor.Theme.t -> string -> document
val process_hook : document -> int -> OEditor.Rect.t -> document

val render_hook :
  document ->
  OEditor.SdlContext.t ->
  OEditor.Theme.t ->
  OEditor.SdlContext.texture option

val prerender_hook :
  document ->
  OEditor.SdlContext.t ->
  OEditor.Helpers.point ->
  OEditor.Helpers.size ->
  document

val postrender_hook : document -> document
val event_hook : document -> Tsdl.Sdl.event -> document
val destroy : document -> unit
val get_changed : document -> bool
val set_changed : document -> bool -> document