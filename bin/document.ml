open Tsdl
open Tsdl_ttf
open Helpers

let scroll_speed = 20
let default_bg_color = Sdl.Color.create ~r:0x33 ~g:0x33 ~b:0x33 ~a:0xff

type document = {
  lines : string list;
  font : Ttf.font;
  bg : Sdl.color;
  cursor : Cursor.cursor;
  scroll_offset : Helpers.point;
  viewport_size : Helpers.size;
  viewport_offset : Helpers.point;
  text_changed : bool;
  cached_texture : Sdl.texture option;
  shift_pressed : bool;
  ctrl_pressed : bool;
}

let create_empty font =
  {
    lines = [ "" ];
    font;
    bg = default_bg_color;
    cursor = Cursor.create ();
    scroll_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
    viewport_offset = { x = 0; y = 0 };
    text_changed = true;
    cached_texture = None;
    shift_pressed = false;
    ctrl_pressed = false;
  }

let create_from_string font text = { (create_empty font) with lines = [ text ] }

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
  { (create_empty font) with lines = List.rev (read_lines_from_file []) }

let get_column_from_pixel doc x line =
  let normalised_x = x + doc.scroll_offset.x - doc.viewport_offset.x in
  let rec find_column col =
    let this_line = List.nth doc.lines line in
    if col > String.length this_line then String.length this_line
    else
      Ttf.size_utf8 doc.font (String.sub this_line 0 col) >>= fun (w, _) ->
      if normalised_x <= w then if col <> 0 then col - 1 else col
      else find_column (col + 1)
  in
  find_column 0

let get_line_from_pixel doc y =
  let normalised_y = y + doc.scroll_offset.y - doc.viewport_offset.y in
  clamp (normalised_y / Ttf.font_height doc.font) 0 (List.length doc.lines - 1)

let convert_mouse_pos_to_cursor_pos doc pos =
  let line = get_line_from_pixel doc pos.y in
  let column = get_column_from_pixel doc pos.x line in
  { x = column; y = line }

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
        ~x:(-document.scroll_offset.x)
        ~y:(-document.scroll_offset.y + (line_height * line_idx))
        ~w:(Sdl.Rect.w src_size) ~h:(Sdl.Rect.h src_size)
    in
    Sdl.render_copy ~src:src_size ~dst:dst_size renderer texture >>= fun () ->
    Sdl.destroy_texture texture

let get_num_visible_lines document =
  match document.viewport_size.h with
  | 0 -> 0
  | h -> h / Ttf.font_height document.font

let get_first_visible_line document =
  document.scroll_offset.y / Ttf.font_height document.font

let get_last_visible_line document =
  let line = get_first_visible_line document + get_num_visible_lines document in
  clamp (line + 1) 0 (List.length document.lines)

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
  let scroll_margin = Ttf.font_height document.font in
  let font_height = Ttf.font_height document.font in
  let desired_line = Cursor.get_line document.cursor in
  let first_visible_line = document.scroll_offset.y / font_height in
  let last_visible_line = first_visible_line + get_num_visible_lines document in
  let first_line =
    if desired_line < first_visible_line then Some desired_line
    else if desired_line >= last_visible_line then
      Some (desired_line - get_num_visible_lines document + 1)
    else None
  in
  let y =
    match first_line with
    | Some line -> line * font_height
    | None -> document.scroll_offset.y
  in
  (* Attempt to get a vertical viewport margin *)
  (* let y =
       match y with
       | n when n < document.scroll_offset.y + scroll_margin ->
           if Cursor.get_line document.cursor <> 0 then n + scroll_margin else n
       | n
         when n
              > document.scroll_offset.y + document.viewport_size.h - scroll_margin
         ->
           n - scroll_margin
       | _ -> y
     in *)
  let x =
    let text_width =
      get_width_of_text document.font
        (String.sub
           (get_current_line document)
           0
           (Cursor.get_column document.cursor))
    in
    if text_width > document.scroll_offset.x + document.viewport_size.w then
      text_width - document.viewport_size.w + scroll_margin
    else if text_width < document.scroll_offset.x then
      text_width - scroll_margin
    else document.scroll_offset.x
  in
  { document with scroll_offset = { x; y }; text_changed = true }

let process_hook document now (dst_rect : Sdl.rect) =
  {
    document with
    cursor = Cursor.process_hook document.cursor now;
    viewport_size = { w = Sdl.Rect.w dst_rect; h = Sdl.Rect.h dst_rect };
  }

let prerender_hook doc renderer offset size pixel_format =
  let doc =
    match
      (doc.text_changed || Cursor.is_dirty doc.cursor, doc.cached_texture)
    with
    | true, Some texture ->
        Sdl.destroy_texture texture;
        { doc with cached_texture = None }
    | _ -> doc
  in
  let doc =
    match doc.cached_texture with
    | None ->
        let new_texture =
          Sdl.create_texture renderer pixel_format Sdl.Texture.access_target
            ~w:size.w ~h:size.h
          >>= fun texture -> texture
        in
        { doc with cached_texture = Some new_texture }
    | _ -> doc
  in
  { doc with viewport_size = size; viewport_offset = offset }

let render_hook doc renderer font =
  if doc.text_changed || Cursor.is_dirty doc.cursor then (
    Sdl.set_render_target renderer doc.cached_texture >>= fun () ->
    Sdl.set_render_draw_color renderer (Sdl.Color.r doc.bg) (Sdl.Color.g doc.bg)
      (Sdl.Color.b doc.bg) (Sdl.Color.a doc.bg)
    >>= fun () ->
    Sdl.render_fill_rect renderer None >>= fun () ->
    draw_all_lines doc renderer font;
    Cursor.render_hook doc.cursor doc.lines doc.scroll_offset renderer font);
  doc.cached_texture

let postrender_hook doc =
  { doc with text_changed = false; cursor = Cursor.postrender_hook doc.cursor }

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
    text_changed = true;
  }

let remove_selection doc =
  match Cursor.get_selection doc.cursor with
  | Some ((frow, fcol), (srow, scol)) ->
      (* remove entirely selected rows*)
      let rec row_remover doc row =
        if row <= frow then doc
        else
          row_remover { doc with lines = remove doc.lines (frow + 1) } (row - 1)
      in
      let doc = row_remover doc (srow - 1) in

      (* remove text from partially selected rows *)
      let doc =
        if frow = srow then
          let line = List.nth doc.lines frow in
          let line =
            String.cat (String.sub line 0 fcol)
              (String.sub line scol (String.length line - scol))
          in
          { doc with lines = replace doc.lines frow line }
        else
          let first_line = List.nth doc.lines frow in
          let second_line = List.nth doc.lines (frow + 1) in
          let changed_line =
            String.cat
              (String.sub first_line 0 fcol)
              (String.sub second_line scol (String.length second_line - scol))
          in
          let lines = remove doc.lines (frow + 1) in
          let lines = replace lines frow changed_line in
          { doc with lines }
      in

      let cursor = Cursor.select_none doc.cursor in
      let cursor = Cursor.set_line cursor doc.lines frow in
      let cursor = Cursor.set_column cursor doc.lines fcol in
      { doc with cursor }
  | _ -> doc

let insert_or_replace_text_at_cursor doc text =
  let doc =
    if Cursor.has_selection doc.cursor then remove_selection doc else doc
  in
  insert_text_at_cursor doc text

let insert_newline_at_cursor doc =
  let doc = remove_selection doc in
  let changed_line, new_line =
    split_string_at (get_current_line doc) (Cursor.get_column doc.cursor)
  in
  let lines = replace doc.lines (Cursor.get_line doc.cursor) changed_line in
  let lines = insert_after lines (Cursor.get_line doc.cursor) new_line in
  let cursor = Cursor.set_column doc.cursor lines 0 in
  let cursor = Cursor.set_line_rel cursor lines 1 in
  { doc with lines; cursor; text_changed = true }

let remove_char_after_cursor document =
  if Cursor.has_selection document.cursor then remove_selection document
  else if
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
      { document with lines; text_changed = true }
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
    { document with lines; text_changed = true }

let remove_char_before_cursor document =
  if Cursor.has_selection document.cursor then remove_selection document
  else if Cursor.get_column document.cursor = 0 then
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
    { document with lines; cursor; text_changed = true }

let event_hook document e =
  match Sdl.Event.enum Sdl.Event.(get e typ) with
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
      if document.shift_pressed then
        {
          document with
          cursor =
            Cursor.set_selection_end_rel document.cursor document.lines (0, -1);
        }
      else
        let cursor = Cursor.select_none document.cursor in
        scroll_cursor_into_view
          {
            document with
            cursor = Cursor.set_column_rel cursor document.lines (-1);
          }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
      if document.shift_pressed then
        {
          document with
          cursor =
            Cursor.set_selection_end_rel document.cursor document.lines (0, 1);
        }
      else
        let cursor = Cursor.select_none document.cursor in
        scroll_cursor_into_view
          {
            document with
            cursor = Cursor.set_column_rel cursor document.lines 1;
          }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
      if document.shift_pressed then
        {
          document with
          cursor =
            Cursor.set_selection_end_rel document.cursor document.lines (-1, 0);
        }
      else
        let cursor = Cursor.select_none document.cursor in
        scroll_cursor_into_view
          {
            document with
            cursor = Cursor.set_line_rel cursor document.lines (-1);
          }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
      if document.shift_pressed then
        {
          document with
          cursor =
            Cursor.set_selection_end_rel document.cursor document.lines (1, 0);
        }
      else
        let cursor = Cursor.select_none document.cursor in
        scroll_cursor_into_view
          { document with cursor = Cursor.set_line_rel cursor document.lines 1 }
  | `Key_down
    when Sdl.Event.(get e keyboard_keycode) = Sdl.K.a && document.ctrl_pressed
    ->
      {
        document with
        cursor = Cursor.select_all document.cursor document.lines;
      }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.home ->
      let cursor = Cursor.select_none document.cursor in
      scroll_cursor_into_view
        { document with cursor = Cursor.set_column cursor document.lines 0 }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.kend ->
      let cursor = Cursor.select_none document.cursor in
      scroll_cursor_into_view
        {
          document with
          cursor =
            Cursor.set_column cursor document.lines
              (String.length (get_current_line document));
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.pageup ->
      let cursor = Cursor.select_none document.cursor in
      scroll_cursor_into_view
        {
          document with
          cursor =
            Cursor.set_line_rel cursor document.lines
              (-get_num_visible_lines document);
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.pagedown ->
      let cursor = Cursor.select_none document.cursor in
      scroll_cursor_into_view
        {
          document with
          cursor =
            Cursor.set_line_rel cursor document.lines
              (get_num_visible_lines document);
        }
  | `Key_down when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lshift ->
      { document with shift_pressed = true }
  | `Key_up when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lshift ->
      { document with shift_pressed = false }
  | `Key_down when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lctrl ->
      { document with ctrl_pressed = true }
  | `Key_up when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lctrl ->
      { document with ctrl_pressed = false }
  | `Mouse_wheel ->
      {
        document with
        scroll_offset =
          scroll_to document
            {
              x =
                document.scroll_offset.x
                + (scroll_speed * -Sdl.Event.(get e mouse_wheel_x));
              y =
                document.scroll_offset.y
                + (scroll_speed * -Sdl.Event.(get e mouse_wheel_y));
            };
        text_changed = true;
      }
  | `Mouse_button_down when Sdl.Event.(get e mouse_button_button) = 1 ->
      let cursor_pos =
        convert_mouse_pos_to_cursor_pos document
          {
            x = Sdl.Event.(get e mouse_button_x);
            y = Sdl.Event.(get e mouse_button_y);
          }
      in
      if not document.shift_pressed then
        let cursor =
          Cursor.set_line document.cursor document.lines cursor_pos.y
        in
        let cursor = Cursor.set_column cursor document.lines cursor_pos.x in
        let cursor = Cursor.select_none cursor in
        scroll_cursor_into_view { document with cursor }
      else
        let cursor =
          Cursor.set_selection_end document.cursor document.lines
            (cursor_pos.y, cursor_pos.x)
        in
        { document with cursor }
  | `Text_input ->
      let text = Sdl.Event.(get e text_editing_text) in
      scroll_cursor_into_view (insert_or_replace_text_at_cursor document text)
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.return ->
      scroll_cursor_into_view (insert_newline_at_cursor document)
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.backspace ->
      scroll_cursor_into_view (remove_char_before_cursor document)
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.delete ->
      scroll_cursor_into_view (remove_char_after_cursor document)
  | _ -> document
