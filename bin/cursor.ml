open Tsdl
open Tsdl_ttf
open Helpers

type cursor = {
  mutable line : int;  (** The current line in the document. *)
  mutable column : int;
      (** The current column in the document. The column may be between 0 and the length of the current line (inclusive). *)
  mutable dirty : bool;
      (** Indicates whether the cursor should be re-rendered this frame. *)
  mutable last_blink_time : int;
      (** The time (in ticks) that the last blink state change occurred. *)
  mutable blink_state : bool;
      (** The current blink state. true indicates that the cursor is being shown. *)
  mutable desired_column : int;
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
  if cursor.dirty then (
    cursor.last_blink_time <- 0;
    cursor.blink_state <- false)
  else ();

  let diff = now - cursor.last_blink_time in
  if diff > 500 then
    {
      column = cursor.column;
      line = cursor.line;
      desired_column = cursor.desired_column;
      last_blink_time = now;
      dirty = false;
      blink_state = not cursor.blink_state;
    }
  else cursor

let render_hook cursor lines renderer font =
  if cursor.blink_state then
    let line_so_far = String.sub (List.nth lines cursor.line) 0 cursor.column in
    let line_height = Ttf.font_height font in
    Ttf.size_text font line_so_far >>= fun (w, h) ->
    Sdl.set_render_draw_color renderer 0xff 0xff 0xff 0xff >>= fun () ->
    Sdl.render_draw_line renderer w
      (cursor.line * line_height)
      w
      ((cursor.line * line_height) + h)
    >>= fun () -> ()
  else ()

let set_line_rel cursor lines rel_line =
  (match cursor.line + rel_line with
  | n when n < 0 ->
      (* we can't go back further than the first line. *)
      cursor.column <- 0;
      cursor.line <- 0;
      cursor.desired_column <- no_desired_column
  | n when n > List.length lines - 1 ->
      cursor.line <- List.length lines - 1;
      cursor.column <- String.length (List.nth lines cursor.line);
      cursor.desired_column <- no_desired_column
  | n when n = List.length lines - 1 -> cursor.line <- List.length lines - 1
  | n -> cursor.line <- n);

  let line_length = String.length (List.nth lines cursor.line) in
  (match
     ( cursor.column <= line_length,
       cursor.desired_column <> no_desired_column,
       cursor.desired_column <= line_length )
   with
  | true, true, true ->
      cursor.column <- cursor.desired_column;
      cursor.desired_column <- no_desired_column
  | _, true, false -> cursor.column <- line_length
  | false, false, _ ->
      cursor.desired_column <- cursor.column;
      cursor.column <- line_length
  | false, true, true ->
      cursor.column <- cursor.desired_column;
      cursor.desired_column <- no_desired_column
  | _ -> ());

  cursor.dirty <- true;
  cursor

let rec set_column_rel cursor lines rel_col =
  (match cursor.column + rel_col with
  | n when n < 0 ->
      (* cursor is going backwards over the start of the line *)
      if cursor.line = 0 then
        (* we can't go back further than this! *)
        cursor.column <- 0
      else (
        (* wrap to the previous line *)
        cursor.line <- cursor.line - 1;
        cursor.column <- String.length (List.nth lines cursor.line);
        set_column_rel cursor lines (rel_col + 1) |> ignore)
  | n when n > String.length (List.nth lines cursor.line) ->
      (* cursor is going forwards over the end of the line *)
      if cursor.line = List.length lines - 1 then
        (* we can't go forward further than this! *)
        cursor.column <- String.length (List.nth lines cursor.line)
      else (
        (* wrap to the next line *)
        cursor.line <- cursor.line + 1;
        cursor.column <- 0;
        set_column_rel cursor lines (rel_col - 1) |> ignore)
  | n -> cursor.column <- n);

  cursor.dirty <- true;
  cursor

let get_column cursor = cursor.column
let get_line cursor = cursor.line
