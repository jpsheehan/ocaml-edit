type t = {
  fg_color : Color.t;
  bg_color : Color.t;
  cursor_color : Color.t;
  selection_color : Color.t;
  text_font : SdlContext.font;
}

let create (font_location, font_size) ~fg ~bg ~cursor ~selection =
  let text_font = SdlContext.font_create font_location font_size in
  {
    text_font;
    fg_color = fg;
    bg_color = bg;
    cursor_color = cursor;
    selection_color = selection;
  }

let get_fg_color t = t.fg_color
let get_bg_color t = t.bg_color
let get_cursor_color t = t.cursor_color
let get_selection_color t = t.selection_color
let get_text_font t = t.text_font
