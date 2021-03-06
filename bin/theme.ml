open Tsdl_ttf
open OEditor.Helpers
open OEditor

type t = {
  fg_color : Color.t;
  bg_color : Color.t;
  selection_color : Color.t;
  text_font : Ttf.font;
}

let create font_location font_size fg_color bg_color selection_color =
  Ttf.open_font font_location font_size >>= fun text_font ->
  { text_font; fg_color; bg_color; selection_color }

let get_fg_color t = t.fg_color
let get_bg_color t = t.bg_color
let get_selection_color t = t.selection_color
let get_text_font t = t.text_font
