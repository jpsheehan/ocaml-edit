open Tsdl
open Tsdl_ttf
open Helpers

type cursor_pos = int * int
(** The row, col pair *)

type cursor = {
  pos : cursor_pos;
  dirty : bool;
      (** Indicates whether the cursor should be re-rendered this frame. *)
  last_blink_time : int;
      (** The time (in ticks) that the last blink state change occurred. *)
  blink_state : bool;
      (** The current blink state. true indicates that the cursor is being shown. *)
  desired_column : int;
      (** The column that the should be returned to (if possible) when changing lines. If no column is specifically wanted, then this is set to no_desired_column. *)
  selection_end : cursor_pos option;
}

let no_desired_column = -1

let create () =
  {
    pos = (0, 0);
    blink_state = false;
    last_blink_time = 0;
    dirty = true;
    desired_column = no_desired_column;
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

let render_caret (row, col) text scroll_offset renderer font =
  let line_so_far = String.sub (Doctext.get_line text row) 0 col in
  let line_height = Ttf.font_height font in
  Ttf.size_text font line_so_far >>= fun (w, h) ->
  Sdl.render_draw_line renderer (w - scroll_offset.x)
    ((row * line_height) - scroll_offset.y)
    (w - scroll_offset.x)
    ((row * line_height) + h - scroll_offset.y)
  >>= fun () -> ()

let compair (ar, ac) (br, bc) =
  if ar < br then -1
  else if ar > br then 1
  else if ac < bc then -1
  else if ac > bc then 1
  else 0

let render_selection a b text scroll_offset renderer font =
  match List.sort compair [ a; b ] with
  | [ (frow, fcol); (srow, scol) ] ->
      Sdl.set_render_draw_color renderer 0x55 0x55 0x55 0xff >>= fun () ->
      Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_add >>= fun () ->
      let rec highlight_line row =
        if row > srow then ()
        else
          let line = Doctext.get_line text row in
          let first_col = if row = frow then fcol else 0 in
          let min_x =
            get_width_of_text font (String.sub line 0 first_col)
            - scroll_offset.x
          in
          let last_col = if row = srow then scol else String.length line in
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
      highlight_line frow;
      Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_none >>= fun () ->
      ()
  | _ -> failwith "Could not compair cursor_pos"

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
      match List.sort compair [ cursor.pos; selection_end ] with
      | [ a; b ] -> Some (a, b)
      | _ -> failwith "Could not compair")

let set_line cursor text row =
  let row = clamp row 0 (Doctext.get_number_of_lines text - 1) in
  let _, col = cursor.pos in
  { cursor with pos = (row, col); dirty = true }

let set_line_rel cursor text rel_line =
  let last_row = Doctext.get_number_of_lines text - 1 in
  let cursor =
    match fst cursor.pos + rel_line with
    | n when n < 0 ->
        (* we can't go back further than the first line. *)
        { cursor with pos = (0, 0); desired_column = no_desired_column }
    | n when n > last_row ->
        (* we can't go forward further than the last line. *)
        {
          cursor with
          pos =
            (last_row, String.length (Doctext.get_line text (fst cursor.pos)));
          desired_column = no_desired_column;
        }
    | n when n = last_row -> { cursor with pos = (last_row, snd cursor.pos) }
    | n -> { cursor with pos = (n, snd cursor.pos) }
  in

  (* now figure out what the column should be *)
  let line_length = String.length (Doctext.get_line text (fst cursor.pos)) in
  let cursor =
    match
      ( snd cursor.pos <= line_length,
        cursor.desired_column <> no_desired_column,
        cursor.desired_column <= line_length )
    with
    | true, true, true ->
        {
          cursor with
          pos = (fst cursor.pos, cursor.desired_column);
          desired_column = no_desired_column;
        }
    | _, true, false -> { cursor with pos = (fst cursor.pos, line_length) }
    | false, false, _ ->
        {
          cursor with
          desired_column = snd cursor.pos;
          pos = (fst cursor.pos, line_length);
        }
    | false, true, true ->
        {
          cursor with
          pos = (fst cursor.pos, cursor.desired_column);
          desired_column = no_desired_column;
        }
    | _ -> cursor
  in

  { cursor with dirty = true }

let set_column cursor text column_idx =
  let line_length = String.length (Doctext.get_line text (fst cursor.pos)) in
  let column =
    if column_idx < 0 then 0
    else if column_idx > line_length then line_length
    else column_idx
  in
  { cursor with dirty = true; pos = (fst cursor.pos, column) }

let rec set_column_rel cursor text rel_col =
  if rel_col = 0 then cursor
  else
    let cursor =
      match snd cursor.pos + rel_col with
      | n when n < 0 ->
          (* cursor is going backwards over the start of the line *)
          if fst cursor.pos = 0 then
            (* we can't go back further than this! *)
            { cursor with pos = (0, 0) }
          else
            (* wrap to the previous line *)
            let row = fst cursor.pos - 1 in
            set_column_rel
              {
                cursor with
                pos = (row, String.length (Doctext.get_line text row));
              }
              text (rel_col - n)
      | n when n > String.length (Doctext.get_line text (fst cursor.pos)) ->
          (* cursor is going forwards over the end of the line *)
          if fst cursor.pos = Doctext.get_number_of_lines text - 1 then
            (* we can't go forward further than this! *)
            {
              cursor with
              pos =
                ( fst cursor.pos,
                  String.length (Doctext.get_line text (fst cursor.pos)) );
            }
          else
            (* wrap to the next line *)
            set_column_rel
              { cursor with pos = (fst cursor.pos + 1, 0) }
              text (rel_col - 1)
      | n -> { cursor with pos = (fst cursor.pos, n) }
    in
    { cursor with dirty = true }

let get_column cursor = snd cursor.pos
let get_line cursor = fst cursor.pos
let is_dirty cursor = cursor.dirty

(* Selection stuff *)

let has_selection cursor =
  match cursor.selection_end with None -> false | _ -> true

let select_none cursor = { cursor with selection_end = None; dirty = true }

let select_all cursor text =
  let row = Doctext.get_number_of_lines text - 1 in
  let col = String.length (Doctext.get_line text row) in
  { cursor with selection_end = Some (row, col); pos = (0, 0); dirty = true }

let set_selection_end cursor text (row, col) =
  let row = min row (Doctext.get_number_of_lines text - 1) in
  let line = Doctext.get_line text row in
  let col = if col > String.length line then String.length line else col in
  if compair (row, col) cursor.pos = 0 then
    { cursor with selection_end = None; dirty = true }
  else { cursor with selection_end = Some (row, col); dirty = true }

let set_selection_end_rel cursor text (row, col) =
  let start_row, start_col =
    match cursor.selection_end with
    | Some selection_end -> selection_end
    | None -> cursor.pos
  in
  set_selection_end cursor text (start_row + row, start_col + col)
