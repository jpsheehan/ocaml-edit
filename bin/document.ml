open Tsdl
open Tsdl_ttf
open OEditor
open OEditor.Helpers

let scroll_speed = 20

type document = {
  text : DocTextCache.t;
  theme : Theme.t;
  cursor : Cursor.cursor;
  scroll_offset : Helpers.point;
  viewport_size : Helpers.size;
  viewport_offset : Helpers.point;
  text_changed : bool;
  cached_texture : Sdl.texture option;
  shift_pressed : bool;
  ctrl_pressed : bool;
  changed_since_load : bool;
}

let create_empty theme =
  {
    text = DocTextCache.create ();
    theme;
    cursor = Cursor.create ();
    scroll_offset = { x = 0; y = 0 };
    viewport_size = { w = 0; h = 0 };
    viewport_offset = { x = 0; y = 0 };
    text_changed = true;
    cached_texture = None;
    shift_pressed = false;
    ctrl_pressed = false;
    changed_since_load = false;
  }

let create_from_string font str =
  { (create_empty font) with text = DocTextCache.create_from_string str }

let create_from_file font filename =
  { (create_empty font) with text = DocTextCache.create_from_file filename }

let destroy document =
  let _document =
    { document with text = DocTextCache.flush_textures document.text }
  in
  ()

let get_max_texture_width doc =
  (* this is a pure hack. I guess this is called after the texture has been destroyed but before it is created with the new data *)
  let current_line_width =
    Font.get_width_of_text
      (Theme.get_text_font doc.theme)
      (String.sub
         (DocTextCache.get_line doc.text (Cursor.get_line doc.cursor))
         0
         (Cursor.get_column doc.cursor))
  in
  max current_line_width (DocTextCache.get_max_texture_width doc.text)

let set_changed document changed =
  { document with changed_since_load = changed }

let get_changed document = document.changed_since_load

let get_column_from_pixel doc x row =
  let normalised_x = x + doc.scroll_offset.x - doc.viewport_offset.x in
  let rec find_column col =
    let line = DocTextCache.get_line doc.text row in
    if col > String.length line then String.length line
    else
      Ttf.size_utf8 (Theme.get_text_font doc.theme) (String.sub line 0 col)
      >>= fun (w, _) ->
      if normalised_x <= w then if col <> 0 then col - 1 else col
      else find_column (col + 1)
  in
  find_column 0

let get_line_from_pixel doc y =
  let normalised_y = y + doc.scroll_offset.y - doc.viewport_offset.y in
  clamp
    (normalised_y / Ttf.font_height (Theme.get_text_font doc.theme))
    0
    (DocTextCache.get_number_of_lines doc.text - 1)

let convert_mouse_pos_to_cursor_pos doc pos =
  let line = get_line_from_pixel doc pos.y in
  let column = get_column_from_pixel doc pos.x line in
  { x = column; y = line }

let get_current_line doc =
  DocTextCache.get_line doc.text (Cursor.get_line doc.cursor)

let draw_line_of_text doc renderer font line_idx =
  let line = DocTextCache.get_line doc.text line_idx in
  if String.length line > 0 then
    let line_height = Ttf.font_height font in
    match DocTextCache.get_texture doc.text line_idx with
    | Some (texture, src_size) ->
        let dst_size =
          Sdl.Rect.create ~x:(-doc.scroll_offset.x)
            ~y:(-doc.scroll_offset.y + (line_height * line_idx))
            ~w:(Sdl.Rect.w src_size) ~h:(Sdl.Rect.h src_size)
        in
        Sdl.render_copy ~src:src_size ~dst:dst_size renderer texture
        >>= fun () -> ()
    | None -> ()

let get_num_visible_lines doc =
  match doc.viewport_size.h with
  | 0 -> 0
  | h -> h / Ttf.font_height (Theme.get_text_font doc.theme)

let get_first_visible_line doc =
  max 0 (doc.scroll_offset.y / Ttf.font_height (Theme.get_text_font doc.theme))

let get_last_visible_line doc =
  let line = get_first_visible_line doc + get_num_visible_lines doc in
  clamp (line + 1) 0 (DocTextCache.get_number_of_lines doc.text)

let draw_visible_lines doc renderer font =
  for idx = get_first_visible_line doc to get_last_visible_line doc - 1 do
    draw_line_of_text doc renderer font idx
  done

let scroll_to doc point =
  let font_height = Ttf.font_height (Theme.get_text_font doc.theme) in
  let max_y =
    max 0
      ((DocTextCache.get_number_of_lines doc.text - get_num_visible_lines doc)
      * font_height)
  in
  let y = clamp point.y 0 max_y in
  let max_x =
    max 0 (get_max_texture_width doc - doc.viewport_size.w + (2 * font_height))
  in
  let x = clamp point.x 0 max_x in
  { x; y }

let scroll_cursor_into_view doc =
  let font_height = Ttf.font_height (Theme.get_text_font doc.theme) in
  let scroll_margin = 2 * font_height in
  let desired_line = Cursor.get_line doc.cursor in
  let first_visible_line = doc.scroll_offset.y / font_height in
  let last_visible_line = first_visible_line + get_num_visible_lines doc in
  let first_line =
    if desired_line < first_visible_line then Some desired_line
    else if desired_line >= last_visible_line then
      Some (desired_line - get_num_visible_lines doc + 1)
    else None
  in
  let y =
    match first_line with
    | Some line -> line * font_height
    | None -> doc.scroll_offset.y
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
    let text_width_at_cursor =
      Font.get_width_of_text
        (Theme.get_text_font doc.theme)
        (String.sub (get_current_line doc) 0 (Cursor.get_column doc.cursor))
    in
    if
      text_width_at_cursor
      > doc.scroll_offset.x + doc.viewport_size.w - scroll_margin
    then
      (* we should scroll to the right so the cursor is in frame *)
      text_width_at_cursor - doc.viewport_size.w + scroll_margin
    else if text_width_at_cursor < doc.scroll_offset.x + scroll_margin then
      (* we should scroll to the left so the cursor is in frame *)
      text_width_at_cursor - scroll_margin
    else doc.scroll_offset.x
  in
  { doc with scroll_offset = scroll_to doc { x; y }; text_changed = true }

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
  let doc = { doc with viewport_size = size; viewport_offset = offset } in
  {
    doc with
    text =
      DocTextCache.prepare_textures doc.text renderer
        (Theme.get_text_font doc.theme)
        doc.cursor
        (Theme.get_fg_color doc.theme)
        (Theme.get_bg_color doc.theme)
        (get_first_visible_line doc)
        (get_last_visible_line doc + 1);
  }

let render_hook doc ctx theme =
  if doc.text_changed || Cursor.is_dirty doc.cursor then (
    SdlContext.set_target ctx doc.cached_texture;
    SdlContext.set_draw_color ctx (Theme.get_bg_color doc.theme);
    SdlContext.fill_rect ctx None;
    draw_visible_lines doc renderer (Theme.get_text_font theme);
    Cursor.render_hook doc.cursor
      (DocTextCache.get_text doc.text)
      doc.scroll_offset renderer
      (Theme.get_text_font theme));
  doc.cached_texture

let postrender_hook doc =
  { doc with text_changed = false; cursor = Cursor.postrender_hook doc.cursor }

let insert_text_at_cursor doc new_text =
  let before, after =
    split_string_at (get_current_line doc) (Cursor.get_column doc.cursor)
  in
  let line = String.cat (String.cat before new_text) after in
  let text =
    DocTextCache.replace_line doc.text (Cursor.get_line doc.cursor) line
  in
  {
    doc with
    changed_since_load = true;
    text;
    cursor =
      Cursor.set_column_rel doc.cursor
        (DocTextCache.get_text text)
        (String.length new_text);
    text_changed = true;
  }

let remove_selection doc =
  match Cursor.get_selection doc.cursor with
  | Some (first, second) ->
      (* remove entirely selected rows*)
      let first_row = CursorPos.get_row first in
      let first_col = CursorPos.get_col first in
      let second_row = CursorPos.get_row second in
      let second_col = CursorPos.get_col second in
      let rec row_remover doc row =
        if row <= first_row then doc
        else
          row_remover
            {
              doc with
              text = DocTextCache.remove_line doc.text (first_row + 1);
            }
            (row - 1)
      in
      let doc = row_remover doc (second_row - 1) in

      (* remove text from partially selected rows *)
      let doc =
        if first_row = second_row then
          let line = DocTextCache.get_line doc.text first_row in
          let line =
            String.cat
              (String.sub line 0 first_col)
              (String.sub line second_col (String.length line - second_col))
          in
          {
            doc with
            text = DocTextCache.replace_line doc.text first_row line;
            changed_since_load = true;
          }
        else
          let first_line = DocTextCache.get_line doc.text first_row in
          let second_line = DocTextCache.get_line doc.text (first_row + 1) in
          let changed_line =
            String.cat
              (String.sub first_line 0 first_col)
              (String.sub second_line second_col
                 (String.length second_line - second_col))
          in
          let text = DocTextCache.remove_line doc.text (first_row + 1) in
          let text = DocTextCache.replace_line text first_row changed_line in
          { doc with text; changed_since_load = true }
      in

      let cursor = Cursor.select_none doc.cursor in
      let cursor =
        Cursor.set_line cursor (DocTextCache.get_text doc.text) first_row
      in
      let cursor =
        Cursor.set_column cursor (DocTextCache.get_text doc.text) first_col
      in
      { doc with cursor; changed_since_load = true }
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
  let text =
    DocTextCache.replace_line doc.text (Cursor.get_line doc.cursor) changed_line
  in
  let text =
    DocTextCache.insert_line_after text (Cursor.get_line doc.cursor) new_line
  in
  let cursor = Cursor.set_column doc.cursor (DocTextCache.get_text text) 0 in
  let cursor = Cursor.set_line_rel cursor (DocTextCache.get_text text) 1 in
  { doc with text; cursor; text_changed = true; changed_since_load = true }

let remove_char_after_cursor doc =
  if Cursor.has_selection doc.cursor then remove_selection doc
  else if Cursor.get_column doc.cursor = String.length (get_current_line doc)
  then
    if
      Cursor.get_line doc.cursor = DocTextCache.get_number_of_lines doc.text - 1
    then doc
    else
      (* Delete over newline *)
      let changed_line =
        String.cat (get_current_line doc)
          (DocTextCache.get_line doc.text (Cursor.get_line doc.cursor + 1))
      in
      let text =
        DocTextCache.replace_line doc.text
          (Cursor.get_line doc.cursor)
          changed_line
      in
      let text =
        DocTextCache.remove_line text (Cursor.get_line doc.cursor + 1)
      in
      { doc with text; text_changed = true; changed_since_load = true }
  else
    (* Delete in middle of line *)
    let before, after =
      split_string_at (get_current_line doc) (Cursor.get_column doc.cursor)
    in
    let changed_line =
      String.cat before (String.sub after 1 (String.length after - 1))
    in
    let text =
      DocTextCache.replace_line doc.text
        (Cursor.get_line doc.cursor)
        changed_line
    in
    { doc with text; text_changed = true; changed_since_load = true }

let remove_char_before_cursor doc =
  if Cursor.has_selection doc.cursor then remove_selection doc
  else if Cursor.get_column doc.cursor = 0 then
    if Cursor.get_line doc.cursor = 0 then doc
      (* cannot backspace before the start of the document *)
    else
      let cursor =
        Cursor.set_line_rel doc.cursor (DocTextCache.get_text doc.text) (-1)
      in
      let cursor =
        Cursor.set_column cursor
          (DocTextCache.get_text doc.text)
          (String.length
             (DocTextCache.get_line doc.text (Cursor.get_line cursor)))
      in
      let changed_line =
        String.cat
          (DocTextCache.get_line doc.text (Cursor.get_line doc.cursor - 1))
          (get_current_line doc)
      in
      let text =
        DocTextCache.replace_line doc.text
          (Cursor.get_line doc.cursor - 1)
          changed_line
      in
      let text = DocTextCache.remove_line text (Cursor.get_line doc.cursor) in
      scroll_cursor_into_view { doc with text; cursor }
  else
    let before, after =
      split_string_at (get_current_line doc) (Cursor.get_column doc.cursor)
    in
    let changed_line =
      String.cat (String.sub before 0 (String.length before - 1)) after
    in
    let text =
      DocTextCache.replace_line doc.text
        (Cursor.get_line doc.cursor)
        changed_line
    in
    let cursor =
      Cursor.set_column_rel doc.cursor (DocTextCache.get_text text) (-1)
    in
    { doc with text; cursor; text_changed = true }

let event_hook doc e =
  match Sdl.Event.enum Sdl.Event.(get e typ) with
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
      scroll_cursor_into_view
        (if doc.shift_pressed then
         {
           doc with
           cursor =
             Cursor.set_selection_end_rel doc.cursor
               (DocTextCache.get_text doc.text)
               (CursorPos.create 0 (-1));
         }
        else
          let cursor = Cursor.select_none doc.cursor in
          {
            doc with
            cursor =
              Cursor.set_column_rel cursor (DocTextCache.get_text doc.text) (-1);
          })
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
      scroll_cursor_into_view
        (if doc.shift_pressed then
         {
           doc with
           cursor =
             Cursor.set_selection_end_rel doc.cursor
               (DocTextCache.get_text doc.text)
               (CursorPos.create 0 1);
         }
        else
          let cursor = Cursor.select_none doc.cursor in
          {
            doc with
            cursor =
              Cursor.set_column_rel cursor (DocTextCache.get_text doc.text) 1;
          })
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
      scroll_cursor_into_view
        (if doc.shift_pressed then
         {
           doc with
           cursor =
             Cursor.set_selection_end_rel doc.cursor
               (DocTextCache.get_text doc.text)
               (CursorPos.create (-1) 0);
         }
        else
          let cursor = Cursor.select_none doc.cursor in
          {
            doc with
            cursor =
              Cursor.set_line_rel cursor (DocTextCache.get_text doc.text) (-1);
          })
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
      scroll_cursor_into_view
        (if doc.shift_pressed then
         {
           doc with
           cursor =
             Cursor.set_selection_end_rel doc.cursor
               (DocTextCache.get_text doc.text)
               (CursorPos.create 1 0);
         }
        else
          let cursor = Cursor.select_none doc.cursor in

          {
            doc with
            cursor =
              Cursor.set_line_rel cursor (DocTextCache.get_text doc.text) 1;
          })
  | `Key_down
    when Sdl.Event.(get e keyboard_keycode) = Sdl.K.a && doc.ctrl_pressed ->
      {
        doc with
        cursor = Cursor.select_all doc.cursor (DocTextCache.get_text doc.text);
      }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.home ->
      let cursor = Cursor.select_none doc.cursor in
      scroll_cursor_into_view
        {
          doc with
          cursor = Cursor.set_column cursor (DocTextCache.get_text doc.text) 0;
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.kend ->
      let cursor = Cursor.select_none doc.cursor in
      scroll_cursor_into_view
        {
          doc with
          cursor =
            Cursor.set_column cursor
              (DocTextCache.get_text doc.text)
              (String.length (get_current_line doc));
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.pageup ->
      let cursor = Cursor.select_none doc.cursor in
      scroll_cursor_into_view
        {
          doc with
          cursor =
            Cursor.set_line_rel cursor
              (DocTextCache.get_text doc.text)
              (-get_num_visible_lines doc);
        }
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.pagedown ->
      let cursor = Cursor.select_none doc.cursor in
      scroll_cursor_into_view
        {
          doc with
          cursor =
            Cursor.set_line_rel cursor
              (DocTextCache.get_text doc.text)
              (get_num_visible_lines doc);
        }
  | `Key_down when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lshift ->
      { doc with shift_pressed = true }
  | `Key_up when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lshift ->
      { doc with shift_pressed = false }
  | `Key_down when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lctrl ->
      { doc with ctrl_pressed = true }
  | `Key_up when Sdl.Event.(get e keyboard_scancode) = Sdl.Scancode.lctrl ->
      { doc with ctrl_pressed = false }
  | `Mouse_wheel ->
      {
        doc with
        scroll_offset =
          scroll_to doc
            {
              x =
                doc.scroll_offset.x
                + (scroll_speed * -Sdl.Event.(get e mouse_wheel_x));
              y =
                doc.scroll_offset.y
                + (scroll_speed * -Sdl.Event.(get e mouse_wheel_y));
            };
        text_changed = true;
      }
  | `Mouse_button_down when Sdl.Event.(get e mouse_button_button) = 1 ->
      let cursor_pos =
        convert_mouse_pos_to_cursor_pos doc
          {
            x = Sdl.Event.(get e mouse_button_x);
            y = Sdl.Event.(get e mouse_button_y);
          }
      in
      scroll_cursor_into_view
        (if not doc.shift_pressed then
         let cursor =
           Cursor.set_line doc.cursor
             (DocTextCache.get_text doc.text)
             cursor_pos.y
         in
         let cursor =
           Cursor.set_column cursor
             (DocTextCache.get_text doc.text)
             cursor_pos.x
         in
         let cursor = Cursor.select_none cursor in
         { doc with cursor }
        else
          let cursor =
            Cursor.set_selection_end doc.cursor
              (DocTextCache.get_text doc.text)
              (CursorPos.create cursor_pos.y cursor_pos.x)
          in
          { doc with cursor })
  | `Text_input ->
      let text = Sdl.Event.(get e text_editing_text) in
      scroll_cursor_into_view (insert_or_replace_text_at_cursor doc text)
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.return ->
      scroll_cursor_into_view (insert_newline_at_cursor doc)
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.backspace ->
      scroll_cursor_into_view (remove_char_before_cursor doc)
  | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.delete ->
      scroll_cursor_into_view (remove_char_after_cursor doc)
  | _ -> doc
