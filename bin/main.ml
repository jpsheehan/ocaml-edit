open Tsdl
open Tsdl_ttf
open Helpers

let font_location = "/usr/share/fonts/TTF/FiraCode-Regular.ttf"
let font_size = 16

type editor_state = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  font : Ttf.font;
  lines : string list;
  mutable cursor : Cursor.cursor;
  mutable continue : bool;
}

let create_texture_from_text (state : editor_state) (text : string) :
    Sdl.texture * Sdl.rect =
  let fg = Sdl.Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff in
  let bg = Sdl.Color.create ~r:0x00 ~g:0x00 ~b:0x00 ~a:0xff in
  Ttf.render_utf8_shaded state.font text fg bg >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface state.renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  (texture, surface_size)

let draw_line_of_text state line_idx =
  let texture, src_size =
    create_texture_from_text state (List.nth state.lines line_idx)
  in
  let line_height = Ttf.font_height state.font in
  let dst_size =
    Sdl.Rect.create ~x:0 ~y:(line_height * line_idx) ~w:(Sdl.Rect.w src_size)
      ~h:(Sdl.Rect.h src_size)
  in
  Sdl.render_copy ~src:src_size ~dst:dst_size state.renderer texture
  >>= fun () -> Sdl.destroy_texture texture

let draw_all_lines state =
  for idx = 0 to List.length state.lines - 1 do
    draw_line_of_text state idx
  done

let rec main_event_handler state =
  let e = Sdl.Event.create () in
  if Sdl.poll_event (Some e) then
    let state =
      match Sdl.Event.enum Sdl.Event.(get e typ) with
      | (`Quit | `Key_down)
        when Sdl.Event.(get e keyboard_keycode) = Sdl.K.escape ->
          state.continue <- false;
          state
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
          state.cursor <- Cursor.set_column_rel state.cursor state.lines (-1);
          state
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
          state.cursor <- Cursor.set_column_rel state.cursor state.lines 1;
          state
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
          state.cursor <- Cursor.set_line_rel state.cursor state.lines (-1);
          state
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
          state.cursor <- Cursor.set_line_rel state.cursor state.lines 1;
          state
      | _ -> state
    in
    main_event_handler state
  else state

let rec main_loop state =
  match state.continue with
  | false -> ()
  | true ->
      let state = main_event_handler state in
      let now = Int32.to_int (Sdl.get_ticks ()) in
      state.cursor <- Cursor.process_hook state.cursor now;
      Sdl.set_render_draw_color state.renderer 0x00 0x00 0x00 0xff >>= fun () ->
      Sdl.render_clear state.renderer >>= fun () ->
      draw_all_lines state;
      Cursor.render_hook state.cursor state.lines state.renderer state.font;
      Sdl.render_present state.renderer;
      Sdl.delay 10l;
      main_loop state

let main () =
  Sdl.init Sdl.Init.video >>= fun () ->
  Ttf.init () >>= fun () ->
  Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.(shown + resizable)
  >>= fun (w, r) ->
  Sdl.set_window_title w "OCaml Editor";
  Ttf.open_font font_location font_size >>= fun f ->
  main_loop
    {
      window = w;
      renderer = r;
      font = f;
      lines = [ "Hello, OCaml!"; "This is another line!"; "And another..." ];
      continue = true;
      cursor = Cursor.create ();
    };
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Ttf.quit ();
  Sdl.quit ();
  exit 0

let () = main ()