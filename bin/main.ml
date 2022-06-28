open OEditor
open OEditor.Helpers
open Tsdl

let font_location = "/usr/share/fonts/TTF/FiraCode-Regular.ttf"
let font_size = 14
let default_window_title = "Editor"

type editor_state = {
  ctx : SdlContext.t;
  theme : Theme.t;
  document : Document.document;
  document_size : size;
  document_offset : point;
  filename : string option;
  continue : bool;
  ctrl_pressed : bool;
}

let set_window_title state =
  SdlContext.set_title state.ctx
    (match state.filename with
    | Some filename -> filename ^ " - " ^ default_window_title
    | None -> default_window_title)

let build_theme () =
  Theme.create (font_location, font_size)
    (Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff)
    (Color.create ~r:0x22 ~g:0x22 ~b:0x22 ~a:0xff)
    (Color.create ~r:0xff ~g:0x99 ~b:0x99 ~a:0xff)
    (Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff)

let _prompt_to_save state =
  if Document.get_changed state.document then
    if Dialogs.question "Close File" "Do you want to save your changes?" then
      { state with document = Document.set_changed state.document false }
    else state
  else state

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
              set_window_title state;
              {
                state with
                filename = Some filename;
                document = Document.create_from_file state.theme filename;
              }
          | None -> state)
      | `Key_down
        when Sdl.Event.(get e keyboard_keycode) = Sdl.K.n && state.ctrl_pressed
        ->
          Document.destroy state.document;
          SdlContext.set_title state.ctx default_window_title;
          { state with document = Document.create_empty state.theme }
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
      let state = main_event_handler state in
      let start_of_frame = SdlContext.get_ticks () in
      let state =
        {
          state with
          document =
            Document.process_hook state.document start_of_frame
              (SdlContext.get_rect state.ctx);
        }
      in
      SdlContext.set_draw_color state.ctx (Theme.get_bg_color state.theme);

      SdlContext.clear state.ctx;
      let state =
        {
          state with
          document =
            Document.prerender_hook state.document state.ctx
              state.document_offset state.document_size;
        }
      in
      (match Document.render_hook state.document state.ctx state.theme with
      | None -> failwith "texture was None"
      | Some texture ->
          SdlContext.set_target state.ctx None;
          SdlContext.copy state.ctx texture
            ~src:
              (Rect.create ~x:0 ~y:0 ~w:state.document_size.w
                 ~h:state.document_size.h)
            ~dst:
              (Rect.create ~x:state.document_offset.x ~y:state.document_offset.y
                 ~w:state.document_size.w ~h:state.document_size.h);
          SdlContext.set_target state.ctx None);
      let state =
        { state with document = Document.postrender_hook state.document }
      in

      (* Clean up the frame *)
      SdlContext.present state.ctx;
      SdlContext.delay state.ctx 15;

      main_loop state

let main () =
  let ctx = SdlContext.create default_window_title in
  let theme = build_theme () in
  let document = Document.create_empty theme in
  let state =
    {
      ctx;
      theme;
      continue = true;
      document;
      document_size = { w = 620; h = 460 };
      document_offset = { x = 10; y = 10 };
      ctrl_pressed = false;
      filename = None;
    }
  in
  set_window_title state;
  main_loop state;
  SdlContext.destroy state.ctx;
  exit 0

let () = main ()