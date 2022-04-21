type document

val create_empty : unit -> document
val create_from_file : string -> document
val create_from_string : string -> document
val process_hook : document -> int -> Tsdl.Sdl.rect -> document
val render_hook : document -> Tsdl.Sdl.renderer -> Tsdl_ttf.Ttf.font -> unit
val event_hook : document -> Tsdl.Sdl.event -> document