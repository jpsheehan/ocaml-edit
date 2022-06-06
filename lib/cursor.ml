open Tsdl
open Tsdl_ttf
open OEditor
open Helpers

type cursor = {
  pos : CursorPos.t;
  dirty : bool;
      (** Indicates whether the cursor should be re-rendered this frame. *)
  last_blink_time : int;
      (** The time (in ticks) that the last blink state change occurred. *)
  blink_state : bool;
      (** The current blink state. true indicates that the cursor is being shown. *)
  selection_end : CursorPos.t option;
}

let create () =
  {
    pos = CursorPos.create 0 0;
    blink_state = false;
    last_blink_time = 0;
    dirty = true;
    selection_end = None;
  }

let process_hook cursor now =
  let cursor =
    if cursor.dirty then
      { cursor with last_blink_time = 0; blink_state = false }
    else cursor
  in

  let diff = now - cursor.last_blink_time in
  if diff > 500 then
    {
      cursor with
      last_blink_time = now;
      dirty = true;
      blink_state = not cursor.blink_state;
    }
  else cursor

let postrender_hook cursor = { cursor with dirty = false }

let render_caret pos text scroll_offset renderer font =
  let row = CursorPos.get_row pos in
  let line_so_far =
    String.sub (DocText.get_line text row) 0 (CursorPos.get_col pos)
  in
  let line_height = Ttf.font_height font in
  Ttf.size_text font line_so_far >>= fun (w, h) ->
  Sdl.render_draw_line renderer (w - scroll_offset.x)
    ((row * line_height) - scroll_offset.y)
    (w - scroll_offset.x)
    ((row * line_height) + h - scroll_offset.y)
  >>= fun () -> ()

let render_selection a b text scroll_offset renderer font =
  match List.sort CursorPos.compare [ a; b ] with
  | [ primary; secondary ] ->
      Sdl.set_render_draw_color renderer 0x55 0x55 0x55 0xff >>= fun () ->
      Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_add >>= fun () ->
      let rec highlight_line row =
        if row > CursorPos.get_row secondary then ()
        else
          let line = DocText.get_line text row in
          let first_col =
            if row = CursorPos.get_row primary then CursorPos.get_col primary
            else 0
          in
          let min_x =
            get_width_of_text font (String.sub line 0 first_col)
            - scroll_offset.x
          in
          let last_col =
            if row = CursorPos.get_row secondary then
              CursorPos.get_col secondary
            else String.length line
          in
          let max_x =
            get_width_of_text font (String.sub line 0 last_col)
            - scroll_offset.x
          in
          let width = max_x - min_x in
          let height = Ttf.font_height font in
          let y = (height * row) - scroll_offset.y in
          Sdl.render_fill_rect renderer
            (Some (Sdl.Rect.create ~x:min_x ~y ~w:width ~h:height))
          >>= fun () ->
          highlight_line (row + 1);
          ()
      in
      highlight_line (CursorPos.get_row primary);
      Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_none >>= fun () ->
      ()
  | _ -> failwith "Could not compare CursorPos.t"

let render_hook cursor text scroll_offset renderer font =
  match cursor.selection_end with
  | None ->
      if cursor.blink_state then
        Sdl.set_render_draw_color renderer 0xff 0xff 0xff 0xff >>= fun () ->
        render_caret cursor.pos text scroll_offset renderer font
  | Some selection_pos ->
      render_selection cursor.pos selection_pos text scroll_offset renderer font;
      if cursor.blink_state then
        Sdl.set_render_draw_color renderer 0xff 0xff 0xff 0xff >>= fun () ->
        render_caret selection_pos text scroll_offset renderer font

let get_selection cursor =
  match cursor.selection_end with
  | None -> None
  | Some selection_end -> (
      match List.sort CursorPos.compare [ cursor.pos; selection_end ] with
      | [ a; b ] -> Some (a, b)
      | _ -> failwith "Could not compare CursorPos")

let set_line cursor text row =
  { cursor with pos = CursorPos.set_row cursor.pos text row; dirty = true }

let set_line_rel cursor text rel_line =
  {
    cursor with
    dirty = true;
    pos = CursorPos.set_row_rel cursor.pos text rel_line;
  }

let set_column cursor text col =
  let pos = CursorPos.set_col cursor.pos text col in
  { cursor with dirty = true; pos }

let set_column_rel cursor text rel_col =
  let pos = CursorPos.set_col_rel cursor.pos text rel_col in
  { cursor with dirty = true; pos }

let get_column cursor =
  match cursor.selection_end with
  | Some pos -> CursorPos.get_col pos
  | None -> CursorPos.get_col cursor.pos

let get_line cursor =
  match cursor.selection_end with
  | Some pos -> CursorPos.get_row pos
  | None -> CursorPos.get_row cursor.pos

let is_dirty cursor = cursor.dirty

(* Selection stuff *)

let has_selection cursor =
  match cursor.selection_end with None -> false | _ -> true

let select_none cursor = { cursor with selection_end = None; dirty = true }

let select_all cursor text =
  let row = DocText.get_number_of_lines text - 1 in
  let col = String.length (DocText.get_line text row) in
  {
    cursor with
    selection_end = Some (CursorPos.create row col);
    pos = CursorPos.create 0 0;
    dirty = true;
  }

let set_selection_end cursor text pos =
  let row =
    min (CursorPos.get_row pos) (DocText.get_number_of_lines text - 1)
  in
  let line = DocText.get_line text row in
  let col =
    if CursorPos.get_col pos > String.length line then String.length line
    else CursorPos.get_col pos
  in
  let pos = CursorPos.create row col in
  if CursorPos.compare pos cursor.pos = 0 then
    { cursor with selection_end = None; dirty = true }
  else { cursor with selection_end = Some pos; dirty = true }

let set_selection_end_rel cursor text new_end =
  let start_pos =
    match cursor.selection_end with
    | Some selection_end -> selection_end
    | None -> cursor.pos
  in
  let start_pos =
    CursorPos.set_row_rel start_pos text (CursorPos.get_row new_end)
  in
  let start_pos =
    CursorPos.set_col_rel start_pos text (CursorPos.get_col new_end)
  in
  set_selection_end cursor text start_pos
