open OEditor

type t = {
  fg_color : Color.t;
  bg_color : Color.t;
  selection_color : Color.t;
  text_font : SdlContext.font;
}

let create (font_location, font_size) fg_color bg_color selection_color =
  let text_font = SdlContext.font_create font_location font_size in
  { text_font; fg_color; bg_color; selection_color }

let get_fg_color t = t.fg_color
let get_bg_color t = t.bg_color
let get_selection_color t = t.selection_color
let get_text_font t = t.text_font
