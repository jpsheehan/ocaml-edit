open Tsdl
open Tsdl_ttf
open Helpers

let font_location = "/usr/share/fonts/TTF/FiraCode-Regular.ttf"
let font_size = 12
let target_fps = 60
let target_mspf = 1000 / target_fps
let red = Sdl.Color.create ~r:0xff ~g:0x00 ~b:0x00 ~a:0xff
let orange = Sdl.Color.create ~r:0xff ~g:0xff ~b:0x00 ~a:0xff
let green = Sdl.Color.create ~r:0x00 ~g:0xff ~b:0x00 ~a:0xff

type editor_state = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  font : Ttf.font;
  document : Document.document;
  document_size : size;
  document_offset : point;
  continue : bool;
  frame_perfc : int Performance_counter.performance_counter;
}

let rec main_event_handler state =
  let e = Sdl.Event.create () in
  if Sdl.poll_event (Some e) then
    let state =
      match Sdl.Event.enum Sdl.Event.(get e typ) with
      | `Quit -> { state with continue = false }
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.escape ->
          { state with continue = false }
      | `Window_event
        when Sdl.Event.(get e window_event_id) = Sdl.Event.window_event_resized
        ->
          {
            state with
            document_size =
              {
                w =
                  Int32.to_int Sdl.Event.(get e window_data1)
                  - (2 * state.document_offset.x);
                h =
                  Int32.to_int Sdl.Event.(get e window_data2)
                  - (2 * state.document_offset.y);
              };
          }
      | _ -> { state with document = Document.event_hook state.document e }
    in
    main_event_handler state
  else state

let compute_mean xs =
  match List.length xs with
  | 0 -> 0
  | len ->
      let sum = List.fold_left ( + ) 0 xs in
      sum / len

let render_performance_counter renderer font perfc =
  let compute_time = Performance_counter.compute perfc compute_mean in
  let compute_time_str = string_of_int compute_time in
  let compute_time_str = String.cat compute_time_str " ms" in

  let color =
    match compute_time with
    | n when n < int_of_float (float_of_int target_mspf *. 0.8) -> green
    | n when n < target_mspf -> orange
    | _ -> red
  in

  Ttf.render_text_blended font compute_time_str color >>= fun surface ->
  let surface_w, surface_h = Sdl.get_surface_size surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.render_copy
    ~dst:
      (Sdl.Rect.create
         ~x:(Sdl.Rect.w (Sdl.render_get_viewport renderer) - surface_w - 10)
         ~y:10 ~w:surface_w ~h:surface_h)
    renderer texture
  >>= fun () -> Sdl.destroy_texture texture

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
      let state =
        {
          state with
          document =
            Document.prerender_hook state.document state.renderer
              state.document_offset state.document_size
              (Sdl.get_window_pixel_format state.window);
        }
      in
      (match Document.render_hook state.document state.renderer state.font with
      | None -> Printf.printf "Warning: texture was None\n"
      | Some texture ->
          Sdl.set_render_target state.renderer None >>= fun () ->
          Sdl.render_copy
            ~src:
              (Sdl.Rect.create ~x:0 ~y:0 ~w:state.document_size.w
                 ~h:state.document_size.h)
            ~dst:
              (Sdl.Rect.create ~x:state.document_offset.x
                 ~y:state.document_offset.y ~w:state.document_size.w
                 ~h:state.document_size.h)
            state.renderer texture
          >>= fun () ->
          Sdl.set_render_target state.renderer None >>= fun () -> ());
      let state =
        { state with document = Document.postrender_hook state.document }
      in

      (* Do some performance counting *)
      let end_of_frame = Int32.to_int (Sdl.get_ticks ()) in
      let diff = end_of_frame - now in
      let state =
        {
          state with
          frame_perfc = Performance_counter.push state.frame_perfc diff;
        }
      in
      render_performance_counter state.renderer state.font state.frame_perfc;

      (* Clean up the frame *)
      Sdl.render_present state.renderer;
      (match diff with
      | n when n <= 0 -> ()
      | _ -> Sdl.delay (Int32.of_int diff));
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
      frame_perfc = Performance_counter.create target_fps;
    };
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Ttf.quit ();
  Sdl.quit ();
  exit 0

let () = main ()