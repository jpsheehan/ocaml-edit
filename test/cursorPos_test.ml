open OEditor
(* open OEditor.Helpers *)

let sample_single_line_text = DocText.create_from_string "Hello, World"

let sample_multi_line_text =
  DocText.create_from_string "First line\n2nd line\nThird line"

let sample_empty_text = DocText.create ()

(* create *)

let%test "create creates a CursorPos at the origin" =
  let expected = (0, 0) in
  let pos = CursorPos.create 0 0 in
  let actual = (CursorPos.get_row pos, CursorPos.get_col pos) in
  expected = actual

(* set_row *)
let%test "set_row works if the row is in bounds" =
  let expected = CursorPos.create 2 0 in
  let actual = CursorPos.create 0 0 in
  let actual = CursorPos.set_row actual sample_multi_line_text 2 in
  CursorPos.equal actual expected

let%test "set_row clamps if row is less than 0" =
  let expected = CursorPos.create 0 3 in
  let actual = CursorPos.create 0 3 in
  let actual = CursorPos.set_row actual sample_single_line_text (-1) in
  CursorPos.equal actual expected