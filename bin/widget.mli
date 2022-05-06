type 'a widget
type event_args = Tsdl.Sdl.event
type process_args = Helpers.size
type prerender_args = Tsdl.Sdl.rect * Tsdl_ttf.Ttf.font
type render_args = Tsdl.Sdl.renderer

val create :
  'a ->
  ('a -> event_args -> 'a) option ->
  ('a -> process_args -> 'a) option ->
  ('a -> prerender_args -> 'a) option ->
  ('a -> render_args -> unit) option ->
  'a widget

val handle_event : 'a widget -> event_args -> 'a widget
val handle_process : 'a widget -> process_args -> 'a widget
val handle_prerender : 'a widget -> prerender_args -> 'a widget
val handle_render : 'a widget -> render_args -> unit