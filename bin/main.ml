open Tsdl
open Tsdl_ttf
open OEditor
open OEditor.Helpers

let font_location = "/usr/share/fonts/TTF/FiraCode-Regular.ttf"
let font_size = 14
let default_window_title = "Jesse's Editor"

type editor_state = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  theme : Theme.t;
  document : Document.document;
  document_size : size;
  document_offset : point;
  filename : string option;
  continue : bool;
  ctrl_pressed : bool;
  old_changed_state : bool;
}

let set_window_title state =
  let changed_star = if Document.get_changed state.document then "*" else "" in
  Sdl.set_window_title state.window
    (default_window_title ^ " - "
    ^ (match state.filename with
      | Some filename -> filename
      | None -> "<unsaved file>")
    ^ changed_star)

let build_theme () =
  Theme.create font_location font_size
    (Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff)
    (Color.create ~r:0x22 ~g:0x22 ~b:0x22 ~a:0xff)
    (Color.create ~r:0xff ~g:0x99 ~b:0x99 ~a:0xff)

let _prompt_to_save state =
  if Document.get_changed state.document then
    if Dialogs.question "Close File" "Do you want to save your changes?" then
      { state with document = Document.set_changed state.document false }
    else state
  else state

let save_to_file state =
  match state.filename with
  | Some filename ->
      let chan = open_out filename in
      let lines = Document.map_lines state.document (fun x -> x) in
      let content =
        List.fold_left (fun a b -> a ^ "\n" ^ b) (List.hd lines) (List.tl lines)
      in
      output_string chan content
  | None -> ()

let rec main_event_handler state =
  let e = Sdl.Event.create () in
  if Sdl.poll_event (Some e) then
    let state =
      match Sdl.Event.enum Sdl.Event.(get e typ) with
      | `Quit -> { state with continue = false }
      | `Key_down
        when Sdl.Event.(get e keyboard_keycode) = Sdl.K.o && state.ctrl_pressed
        -> (
          (* let state = prompt_to_save state in *)
          match Dialogs.open_file "Open a file" with
          | Some filename ->
              Document.destroy state.document;
              let state =
                {
                  state with
                  filename = Some filename;
                  document = Document.create_from_file state.theme filename;
                }
              in
              set_window_title state;
              state
          | None -> state)
      | `Key_down
        when Sdl.Event.(get e keyboard_keycode) = Sdl.K.s && state.ctrl_pressed
        -> (
          match state.filename with
          | Some _ ->
              save_to_file state;
              let state =
                {
                  state with
                  document = Document.set_changed state.document false;
                }
              in
              set_window_title state;
              state
          | None -> (
              match Dialogs.save_file "Save file" with
              | Some filename ->
                  let state =
                    {
                      state with
                      filename = Some filename;
                      document = Document.set_changed state.document false;
                    }
                  in
                  save_to_file state;
                  set_window_title state;
                  state
              | None -> state))
      | `Key_down
        when Sdl.Event.(get e keyboard_keycode) = Sdl.K.n && state.ctrl_pressed
        ->
          Document.destroy state.document;
          let state =
            {
              state with
              document = Document.create_empty state.theme;
              filename = None;
            }
          in
          set_window_title state;
          state
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.lctrl ->
          let state = { state with ctrl_pressed = true } in
          { state with document = Document.event_hook state.document e }
      | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.rctrl ->
          let state = { state with ctrl_pressed = true } in
          { state with document = Document.event_hook state.document e }
      | `Key_up when Sdl.Event.(get e keyboard_keycode) = Sdl.K.lctrl ->
          let state = { state with ctrl_pressed = false } in
          { state with document = Document.event_hook state.document e }
      | `Key_up when Sdl.Event.(get e keyboard_keycode) = Sdl.K.rctrl ->
          let state = { state with ctrl_pressed = false } in
          { state with document = Document.event_hook state.document e }
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

let rec main_loop state =
  match state.continue with
  | false -> ()
  | true ->
      let start_of_frame = Int32.to_int (Sdl.get_ticks ()) in
      let state = main_event_handler state in
      let state =
        {
          state with
          document =
            Document.process_hook state.document start_of_frame
              (Sdl.render_get_clip_rect state.renderer);
        }
      in
      Helpers.set_render_draw_color state.renderer
        (Theme.get_bg_color state.theme)
      >>= fun () ->
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
      (match Document.render_hook state.document state.renderer state.theme with
      | None -> failwith "texture was None"
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

      (* Clean up the frame *)
      Sdl.render_present state.renderer;
      let end_of_frame = Int32.to_int (Sdl.get_ticks ()) in
      let _diff = end_of_frame - start_of_frame in

      (* Printf.printf "%d\n" diff; *)
      Sdl.delay (Int32.of_int 15);

      let new_changed_state = Document.get_changed state.document in
      if new_changed_state <> state.old_changed_state then
        set_window_title state;
      let state = { state with old_changed_state = new_changed_state } in

      main_loop state

let main () =
  Sdl.init Sdl.Init.video >>= fun () ->
  Ttf.init () >>= fun () ->
  Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.(shown + resizable)
  >>= fun (w, r) ->
  let theme = build_theme () in
  let document = Document.create_empty theme in
  let state =
    {
      window = w;
      renderer = r;
      theme;
      continue = true;
      document;
      document_size = { w = 620; h = 460 };
      document_offset = { x = 10; y = 10 };
      ctrl_pressed = false;
      filename = None;
      old_changed_state = false;
    }
  in
  set_window_title state;
  main_loop state;
  Sdl.destroy_renderer r;
  Sdl.destroy_window w;
  Ttf.quit ();
  Sdl.quit ();
  exit 0

let () = main ()