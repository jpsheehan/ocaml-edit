open Tsdl
open Tsdl_ttf
open Helpers

let scroll_speed = 10

type document = {
  lines : string list;
  font : Ttf.font;
  mutable cursor : Cursor.cursor;
  mutable viewport_offset : Helpers.point;
  mutable viewport_size : Helpers.size;
}

let create_empty font =
  {
    lines = [ "" ];
    font;
    cursor = Cursor.create ();
    viewport_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
  }

let create_from_string font text =
  {
    lines = [ text ];
    font;
    cursor = Cursor.create ();
    viewport_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
  }

let create_from_file font filename =
  let file = open_in filename in
  let rec read_lines_from_file lines =
    try
      let line = input_line file in
      read_lines_from_file (line :: lines)
    with End_of_file ->
      close_in file;
      lines
  in
  {
    font;
    cursor = Cursor.create ();
    lines = List.rev (read_lines_from_file []);
    viewport_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
  }

let create_texture_from_text renderer font text : Sdl.texture * Sdl.rect =
  let fg = Sdl.Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff in
  let bg = Sdl.Color.create ~r:0x00 ~g:0x00 ~b:0x00 ~a:0xff in
  Ttf.render_utf8_shaded font text fg bg >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  (texture, surface_size)

let draw_line_of_text document renderer font line_idx =
  let line = List.nth document.lines line_idx in
  if String.length line > 0 then
    let line_height = Ttf.font_height font in
    let texture, src_size = create_texture_from_text renderer font line in
    let dst_size =
      Sdl.Rect.create
        ~x:(-document.viewport_offset.x)
        ~y:(-document.viewport_offset.y + (line_height * line_idx))
        ~w:(Sdl.Rect.w src_size) ~h:(Sdl.Rect.h src_size)
    in
    Sdl.render_copy ~src:src_size ~dst:dst_size renderer texture >>= fun () ->
    Sdl.destroy_texture texture

let get_num_visible_lines document =
  match document.viewport_size.h with
  | 0 -> 0
  | h -> h / Ttf.font_height document.font

let get_first_visible_line document =
  document.viewport_offset.y / Ttf.font_height document.font

let get_last_visible_line document =
  let line = get_first_visible_line document + get_num_visible_lines document in
  clamp line 0 (List.length document.lines - 1)

let draw_all_lines document renderer font =
  for
    (*idx = 0
      to List.length document.lines - 1*)
    idx = get_first_visible_line document to get_last_visible_line document - 1
  do
    draw_line_of_text document renderer font idx
  done

let scroll_to document point =
  let max_y =
    (List.length document.lines - 1) * Ttf.font_height document.font
  in
  let y = clamp point.y 0 max_y in
  let x = if point.x < 0 then 0 else point.x in
  (* TODO: Add upper limits for x too! *)
  { x; y }

let scroll_cursor_into_view document =
  let font_height = Ttf.font_height document.font in
  let desired_line = Cursor.get_line document.cursor in
  let first_visible_line = document.viewport_offset.y / font_height in
  let last_visible_line = first_visible_line + get_num_visible_lines document in
  let first_line =
    if desired_line < first_visible_line then Some desired_line
    else if desired_line >= last_visible_line then
      Some (desired_line - get_num_visible_lines document)
    else None
  in
  match first_line with
  | None -> document.viewport_offset
  | Some line -> { document.viewport_offset with y = line * font_height }

let process_hook document now (dst_rect : Sdl.rect) =
  (* Printf.printf "Height: %d\n" document.viewport_size.h; *)
  document.cursor <- Cursor.process_hook document.cursor now;
  document.viewport_size <- { w = Sdl.Rect.w dst_rect; h = Sdl.Rect.h dst_rect };
  document

let prerender_hook document renderer _font =
  document.viewport_size <-
    (let r = Sdl.render_get_viewport renderer in
     { w = Sdl.Rect.w r; h = Sdl.Rect.h r });
  document

let render_hook document renderer font =
  Sdl.render_fill_rect renderer None >>= fun () ->
  draw_all_lines document renderer font;
  Cursor.render_hook document.cursor document.lines document.viewport_offset
    renderer font

let event_hook document e =
  match Sdl.Event.enum Sdl.Event.(get e typ) with
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
      document.cursor <-
        Cursor.set_column_rel document.cursor document.lines (-1);
      document.viewport_offset <- scroll_cursor_into_view document;
      document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
      document.cursor <- Cursor.set_column_rel document.cursor document.lines 1;
      document.viewport_offset <- scroll_cursor_into_view document;
      document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
      document.cursor <- Cursor.set_line_rel document.cursor document.lines (-1);
      document.viewport_offset <- scroll_cursor_into_view document;
      document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
      document.cursor <- Cursor.set_line_rel document.cursor document.lines 1;
      document.viewport_offset <- scroll_cursor_into_view document;
      document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.home ->
      document.cursor <- Cursor.set_column document.cursor document.lines 0;
      document.viewport_offset <- scroll_cursor_into_view document;
      document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.kend ->
      document.cursor <-
        Cursor.set_column document.cursor document.lines
          (String.length
             (List.nth document.lines (Cursor.get_line document.cursor)));
      document.viewport_offset <- scroll_cursor_into_view document;
      document
  | `Mouse_wheel ->
      document.viewport_offset <-
        scroll_to document
          {
            document.viewport_offset with
            y =
              document.viewport_offset.y
              + (scroll_speed * -Sdl.Event.(get e mouse_wheel_y));
          };
      document
  | _ -> document
