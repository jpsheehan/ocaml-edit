open Tsdl
open Tsdl_ttf
open Helpers

let scroll_speed = 10
let default_bg_color = Sdl.Color.create ~r:0x33 ~g:0x33 ~b:0x33 ~a:0xff

type document = {
  lines : string list;
  font : Ttf.font;
  bg : Sdl.color;
  cursor : Cursor.cursor;
  viewport_offset : Helpers.point;
  viewport_size : Helpers.size;
}

let create_empty font =
  {
    lines = [ "" ];
    font;
    bg = default_bg_color;
    cursor = Cursor.create ();
    viewport_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
  }

let create_from_string font text =
  {
    lines = [ text ];
    font;
    bg = default_bg_color;
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
    bg = default_bg_color;
    cursor = Cursor.create ();
    lines = List.rev (read_lines_from_file []);
    viewport_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
  }

let get_current_line document =
  List.nth document.lines (Cursor.get_line document.cursor)

let create_texture_from_text renderer font text bg : Sdl.texture * Sdl.rect =
  let fg = Sdl.Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff in
  Ttf.render_utf8_shaded font text fg bg >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  (texture, surface_size)

let draw_line_of_text document renderer font line_idx =
  let line = List.nth document.lines line_idx in
  if String.length line > 0 then
    let line_height = Ttf.font_height font in
    let texture, src_size =
      create_texture_from_text renderer font line document.bg
    in
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
  clamp line 0 (List.length document.lines)

let draw_all_lines document renderer font =
  for
    idx = get_first_visible_line document to get_last_visible_line document - 1
  do
    draw_line_of_text document renderer font idx
  done

let scroll_to document point =
  let max_y =
    (List.length document.lines * Ttf.font_height document.font)
    - (get_num_visible_lines document * Ttf.font_height document.font)
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
      Some (desired_line - get_num_visible_lines document + 1)
    else None
  in
  {
    document with
    viewport_offset =
      (match first_line with
      | None -> document.viewport_offset
      | Some line -> { document.viewport_offset with y = line * font_height });
  }

let process_hook document now (dst_rect : Sdl.rect) =
  (* Printf.printf "Height: %d\n" document.viewport_size.h; *)
  {
    document with
    cursor = Cursor.process_hook document.cursor now;
    viewport_size = { w = Sdl.Rect.w dst_rect; h = Sdl.Rect.h dst_rect };
  }

let prerender_hook document renderer _font =
  {
    document with
    viewport_size =
      (let r = Sdl.render_get_viewport renderer in
       { w = Sdl.Rect.w r; h = Sdl.Rect.h r });
  }

let render_hook document renderer font =
  Sdl.set_render_draw_color renderer (Sdl.Color.r document.bg)
    (Sdl.Color.g document.bg) (Sdl.Color.b document.bg)
    (Sdl.Color.a document.bg)
  >>= fun () ->
  Sdl.render_fill_rect renderer None >>= fun () ->
  draw_all_lines document renderer font;
  Cursor.render_hook document.cursor document.lines document.viewport_offset
    renderer font

let insert_text_at_cursor document text =
  let before, after =
    split_string_at
      (get_current_line document)
      (Cursor.get_column document.cursor)
  in
  let line = String.cat (String.cat before text) after in
  let lines = replace document.lines (Cursor.get_line document.cursor) line in
  {
    document with
    lines;
    cursor = Cursor.set_column_rel document.cursor lines (String.length text);
  }

let insert_newline_at_cursor document =
  let changed_line, new_line =
    split_string_at
      (get_current_line document)
      (Cursor.get_column document.cursor)
  in
  let lines =
    replace document.lines (Cursor.get_line document.cursor) changed_line
  in
  let lines = insert_after lines (Cursor.get_line document.cursor) new_line in
  let cursor = Cursor.set_column document.cursor lines 0 in
  let cursor = Cursor.set_line_rel cursor lines 1 in
  { document with lines; cursor }

let remove_char_after_cursor document =
  if
    Cursor.get_column document.cursor
    = String.length (get_current_line document)
  then
    if Cursor.get_line document.cursor = List.length document.lines - 1 then
      document
    else
      (* Delete over newline *)
      let changed_line =
        String.cat
          (get_current_line document)
          (List.nth document.lines (Cursor.get_line document.cursor + 1))
      in
      let lines =
        replace document.lines (Cursor.get_line document.cursor) changed_line
      in
      let lines = remove lines (Cursor.get_line document.cursor + 1) in
      { document with lines }
  else
    (* Delete in middle of line *)
    let before, after =
      split_string_at
        (get_current_line document)
        (Cursor.get_column document.cursor)
    in
    let changed_line =
      String.cat before (String.sub after 1 (String.length after - 1))
    in
    let lines =
      replace document.lines (Cursor.get_line document.cursor) changed_line
    in
    { document with lines }

let remove_char_before_cursor document =
  if Cursor.get_column document.cursor = 0 then
    if Cursor.get_line document.cursor = 0 then document
      (* cannot backspace before the start of the document *)
    else
      let cursor = Cursor.set_line_rel document.cursor document.lines (-1) in
      let cursor =
        Cursor.set_column cursor document.lines
          (String.length (List.nth document.lines (Cursor.get_line cursor)))
      in
      let changed_line =
        String.cat
          (List.nth document.lines (Cursor.get_line document.cursor - 1))
          (get_current_line document)
      in
      let lines =
        replace document.lines
          (Cursor.get_line document.cursor - 1)
          changed_line
      in
      let lines = remove lines (Cursor.get_line document.cursor) in
      scroll_cursor_into_view { document with lines; cursor }
  else
    let before, after =
      split_string_at
        (get_current_line document)
        (Cursor.get_column document.cursor)
    in
    let changed_line =
      String.cat (String.sub before 0 (String.length before - 1)) after
    in
    let lines =
      replace document.lines (Cursor.get_line document.cursor) changed_line
    in
    let cursor = Cursor.set_column_rel document.cursor lines (-1) in
    { document with lines; cursor }

let event_hook document e =
  match Sdl.Event.enum Sdl.Event.(get e typ) with
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
      scroll_cursor_into_view
        {
          document with
          cursor = Cursor.set_column_rel document.cursor document.lines (-1);
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
      scroll_cursor_into_view
        {
          document with
          cursor = Cursor.set_column_rel document.cursor document.lines 1;
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
      scroll_cursor_into_view
        {
          document with
          cursor = Cursor.set_line_rel document.cursor document.lines (-1);
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
      scroll_cursor_into_view
        {
          document with
          cursor = Cursor.set_line_rel document.cursor document.lines 1;
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.home ->
      scroll_cursor_into_view
        {
          document with
          cursor = Cursor.set_column document.cursor document.lines 0;
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.kend ->
      scroll_cursor_into_view
        {
          document with
          cursor =
            Cursor.set_column document.cursor document.lines
              (String.length (get_current_line document));
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.pageup ->
      scroll_cursor_into_view
        {
          document with
          cursor =
            Cursor.set_line_rel document.cursor document.lines
              (-get_num_visible_lines document);
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.pagedown ->
      scroll_cursor_into_view
        {
          document with
          cursor =
            Cursor.set_line_rel document.cursor document.lines
              (get_num_visible_lines document);
        }
  | `Mouse_wheel ->
      {
        document with
        viewport_offset =
          scroll_to document
            {
              document.viewport_offset with
              y =
                document.viewport_offset.y
                + (scroll_speed * -Sdl.Event.(get e mouse_wheel_y));
            };
      }
  | `Text_input ->
      let text = Sdl.Event.(get e text_editing_text) in
      insert_text_at_cursor document text
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.return ->
      insert_newline_at_cursor document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.backspace ->
      remove_char_before_cursor document
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.delete ->
      remove_char_after_cursor document
  | _ -> document
