open Helpers

type t = { row : int; col : int; preferred_col : int option }

let create row col = { row; col; preferred_col = None }
let get_row t = t.row
let get_col t = t.col

let set_row t text row _use_preferred_col =
  let row = clamp row 0 (Doctext.get_number_of_lines text - 1) in
  { t with row }

let set_col t text col _use_preferred_col =
  let line_length = String.length (Doctext.get_line text t.row) in
  let col =
    if col < 0 then 0 else if col > line_length then line_length else col
  in
  { t with col }

let set_row_rel t text row use_preferred_col =
  set_row t text (t.row + row) use_preferred_col

let set_col_rel t text col use_preferred_col =
  set_col t text (t.col + col) use_preferred_col

let compare a b =
  if a.row < b.row then -1
  else if a.row > b.row then 1
  else if a.col < b.col then -1
  else if a.col > b.col then 1
  else 0