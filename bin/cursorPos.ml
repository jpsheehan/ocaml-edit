type t = { row : int; col : int }

let create row col = { row; col }
let get_row t = t.row
let get_col t = t.col
let set_row t text row = t
let set_col t text col = t
let set_row_rel t text row = set_row t text (t.row + row)
let set_col_rel t text col = set_col t text (t.col + col)

let compare a b =
  if a.row < b.row then -1
  else if a.row > b.row then 1
  else if a.col < b.col then -1
  else if a.col > b.col then 1
  else 0