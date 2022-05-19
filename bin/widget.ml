open OEditor

type event_args = Tsdl.Sdl.event
type process_args = Helpers.size
type prerender_args = Tsdl.Sdl.rect * Tsdl_ttf.Ttf.font
type render_args = Tsdl.Sdl.renderer

type 'a widget = {
  inner : 'a;
  handle_event : ('a -> event_args -> 'a) option;
  handle_process : ('a -> process_args -> 'a) option;
  handle_prerender : ('a -> prerender_args -> 'a) option;
  handle_render : ('a -> render_args -> unit) option;
}

let create inner handle_event handle_process handle_prerender handle_render =
  { inner; handle_event; handle_process; handle_prerender; handle_render }

let handle_event widget args =
  match widget.handle_event with
  | Some handler -> { widget with inner = handler widget.inner args }
  | None -> widget

let handle_process widget args =
  match widget.handle_process with
  | Some handler -> { widget with inner = handler widget.inner args }
  | None -> widget

let handle_prerender widget args =
  match widget.handle_prerender with
  | Some handler -> { widget with inner = handler widget.inner args }
  | None -> widget

let handle_render widget args =
  match widget.handle_render with
  | Some handler ->
      handler widget.inner args;
      ()
  | _ -> ()
