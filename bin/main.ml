open Tsdl
open Tsdl_ttf
open Helpers

let font_location = "/usr/share/fonts/TTF/FiraCode-Regular.ttf"
let font_size = 14

type editor_state = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  font : Ttf.font;
  document : Document.document;
  document_size : size;
  document_offset : point;
  continue : bool;
}

let rec main_event_handler state =
  let e = Sdl.Event.create () in
  if Sdl.poll_event (Some e) then
    let state =
      match Sdl.Event.enum Sdl.Event.(get e typ) with
      | `Quit -> { state with continue = false }
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.escape ->
          { state with continue = false }
      | _ -> { state with document = Document.event_hook state.document e }
    in
    main_event_handler state
  else state

let rec main_loop state =
  match state.continue with
  | false -> ()
  | true ->
      let state = main_event_handler state in
      let now = Int32.to_int (Sdl.get_ticks ()) in
      let state =
        {
          state with
          document =
            Document.process_hook state.document now
              (Sdl.render_get_clip_rect state.renderer);
        }
      in
      Sdl.set_render_draw_color state.renderer 0x22 0x22 0x22 0xff >>= fun () ->
      Sdl.render_clear state.renderer >>= fun () ->
      Sdl.create_texture state.renderer
        (Sdl.get_window_pixel_format state.window)
        Sdl.Texture.access_target ~w:620 ~h:460
      >>= fun texture ->
      Sdl.set_render_target state.renderer (Some texture) >>= fun () ->
      let state =
        {
          state with
          document =
            Document.prerender_hook state.document state.renderer
              state.document_offset;
        }
      in
      Document.render_hook state.document state.renderer state.font;
      Sdl.set_render_target state.renderer None >>= fun () ->
      Sdl.render_copy
        ~src:
          (Sdl.Rect.create ~x:0 ~y:0 ~w:state.document_size.w
             ~h:state.document_size.h)
        ~dst:
          (Sdl.Rect.create ~x:state.document_offset.x ~y:state.document_offset.y
             ~w:state.document_size.w ~h:state.document_size.h)
        state.renderer texture
      >>= fun () ->
      Sdl.render_present state.renderer;
      Sdl.destroy_texture texture;
      Sdl.delay 20l;
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
      continue = true;
      document = Document.create_from_file f "./bin/main.ml";
      document_size = { w = 620; h = 460 };
      document_offset = { x = 10; y = 10 };
    };
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Ttf.quit ();
  Sdl.quit ();
  exit 0

let () = main ()