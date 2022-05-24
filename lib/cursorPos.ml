open Helpers

type t = { row : int; col : int; preferred_col : int option }

let create row col = { row; col; preferred_col = None }
let get_row t = t.row
let get_col t = t.col

let set_row pos text row =
  let row = clamp row 0 (DocText.get_number_of_lines text - 1) in
  { pos with row }

let set_col pos text col =
  let line_length = String.length (DocText.get_line text pos.row) in
  let col =
    if col < 0 then 0 else if col > line_length then line_length else col
  in
  { pos with col }

let set_row_rel pos text rel_row =
  let last_row = DocText.get_number_of_lines text - 1 in
  let pos =
    (* normalise the new position *)
    match pos.row + rel_row with
    | new_row when new_row < 0 ->
        (* we can't go back further than the first line. *)
        create 0 0
    | new_row when new_row > last_row ->
        (* we can't go forward further than the last line. *)
        create last_row (String.length (DocText.get_line text pos.row))
    | new_row when new_row = last_row -> set_row pos text last_row
    | new_row -> set_row pos text new_row
  in

  (* now figure out what the column should be *)
  let line_length = String.length (DocText.get_line text pos.row) in
  match pos.preferred_col with
  | None ->
      if pos.col <= line_length then pos
      else { pos with preferred_col = Some pos.col; col = line_length }
  | Some preferred_col ->
      if pos.col <= line_length && preferred_col <= line_length then
        create pos.row preferred_col
      else { pos with col = line_length }

let rec set_col_rel pos text col =
  if col = 0 then pos
  else
    match pos.col + col with
    | n when n < 0 ->
        (* cursor is going backwards over the start of the line *)
        if pos.row = 0 then (* we can't go back further than this! *)
          create 0 0
        else
          (* wrap to the previous line *)
          let row = pos.row - 1 in
          set_col_rel
            (create row (String.length (DocText.get_line text row)))
            text (col - n)
    | n when n > String.length (DocText.get_line text pos.row) ->
        (* cursor is going forwards over the end of the line *)
        if pos.row = DocText.get_number_of_lines text - 1 then
          (* we can't go forward further than this! *)
          create pos.row (String.length (DocText.get_line text pos.row))
        else
          (* wrap to the next line *)
          set_col_rel (create (pos.row + 1) 0) text (col - 1)
    | n -> create pos.row n

let compare a b =
  if a.row < b.row then -1
  else if a.row > b.row then 1
  else if a.col < b.col then -1
  else if a.col > b.col then 1
  else 0

let equal a b = compare a b = 0
let to_string pos = Printf.sprintf "(r:%d, c:%d)" pos.row pos.col