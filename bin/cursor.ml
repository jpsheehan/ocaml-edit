open Tsdl
open Tsdl_ttf
open Helpers

type cursor = {
  line : int;  (** The current line in the document. *)
  column : int;
      (** The current column in the document. The column may be between 0 and the length of the current line (inclusive). *)
  dirty : bool;
      (** Indicates whether the cursor should be re-rendered this frame. *)
  last_blink_time : int;
      (** The time (in ticks) that the last blink state change occurred. *)
  blink_state : bool;
      (** The current blink state. true indicates that the cursor is being shown. *)
  desired_column : int;
      (** The column that the should be returned to (if possible) when changing lines. If no column is specifically wanted, then this is set to no_desired_column. *)
}

let no_desired_column = -1

let create () =
  {
    line = 0;
    column = 0;
    blink_state = false;
    last_blink_time = 0;
    dirty = true;
    desired_column = no_desired_column;
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
      column = cursor.column;
      line = cursor.line;
      desired_column = cursor.desired_column;
      last_blink_time = now;
      dirty = true;
      blink_state = not cursor.blink_state;
    }
  else cursor

let postrender_hook cursor = { cursor with dirty = false }

let render_hook cursor lines viewport_offset renderer font =
  if cursor.blink_state then
    let line_so_far = String.sub (List.nth lines cursor.line) 0 cursor.column in
    let line_height = Ttf.font_height font in
    Ttf.size_text font line_so_far >>= fun (w, h) ->
    Sdl.set_render_draw_color renderer 0xff 0xff 0xff 0xff >>= fun () ->
    Sdl.render_draw_line renderer (w - viewport_offset.x)
      ((cursor.line * line_height) - viewport_offset.y)
      (w - viewport_offset.x)
      ((cursor.line * line_height) + h - viewport_offset.y)
    >>= fun () -> ()
  else ()

let set_line cursor lines line_idx =
  let num_lines = List.length lines in
  let line =
    if line_idx < 0 then 0
    else if line_idx >= num_lines then num_lines - 1
    else line_idx
  in
  { cursor with line; dirty = true }

let set_line_rel cursor lines rel_line =
  let cursor =
    match cursor.line + rel_line with
    | n when n < 0 ->
        (* we can't go back further than the first line. *)
        { cursor with column = 0; line = 0; desired_column = no_desired_column }
    | n when n > List.length lines - 1 ->
        (* we can't go forward further than the last line. *)
        {
          cursor with
          line = List.length lines - 1;
          column = String.length (List.nth lines cursor.line);
          desired_column = no_desired_column;
        }
    | n when n = List.length lines - 1 ->
        { cursor with line = List.length lines - 1 }
    | n -> { cursor with line = n }
  in

  (* now figure out what the column should be *)
  let line_length = String.length (List.nth lines cursor.line) in
  let cursor =
    match
      ( cursor.column <= line_length,
        cursor.desired_column <> no_desired_column,
        cursor.desired_column <= line_length )
    with
    | true, true, true ->
        {
          cursor with
          column = cursor.desired_column;
          desired_column = no_desired_column;
        }
    | _, true, false -> { cursor with column = line_length }
    | false, false, _ ->
        { cursor with desired_column = cursor.column; column = line_length }
    | false, true, true ->
        {
          cursor with
          column = cursor.desired_column;
          desired_column = no_desired_column;
        }
    | _ -> cursor
  in

  { cursor with dirty = true }

let set_column cursor lines column_idx =
  let line_length = String.length (List.nth lines cursor.line) in
  let column =
    if column_idx < 0 then 0
    else if column_idx > line_length then line_length
    else column_idx
  in
  { cursor with dirty = true; column }

let rec set_column_rel cursor lines rel_col =
  if rel_col = 0 then cursor
  else
    let cursor =
      match cursor.column + rel_col with
      | n when n < 0 ->
          (* cursor is going backwards over the start of the line *)
          if cursor.line = 0 then
            (* we can't go back further than this! *)
            { cursor with column = 0 }
          else
            (* wrap to the previous line *)
            let line = cursor.line - 1 in
            set_column_rel
              { cursor with line; column = String.length (List.nth lines line) }
              lines (rel_col - n)
      | n when n > String.length (List.nth lines cursor.line) ->
          (* cursor is going forwards over the end of the line *)
          if cursor.line = List.length lines - 1 then
            (* we can't go forward further than this! *)
            { cursor with column = String.length (List.nth lines cursor.line) }
          else
            (* wrap to the next line *)
            set_column_rel
              { cursor with line = cursor.line + 1; column = 0 }
              lines (rel_col - 1)
      | n -> { cursor with column = n }
    in
    { cursor with dirty = true }

let get_column cursor = cursor.column
let get_line cursor = cursor.line
let is_dirty cursor = cursor.dirty