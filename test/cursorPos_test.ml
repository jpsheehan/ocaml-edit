open OEditor
(* open OEditor.Helpers *)

let sample_single_line_text = DocText.create_from_string "Hello, World"

let sample_multi_line_text =
  DocText.create_from_string "First line\n2nd line\nThird line"

let sample_multi_column_text =
  DocText.create_from_string "11111\n22\n33333\n4444\n"

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

let%test "set_row clamps if row is greater than number of rows" =
  let expected = CursorPos.create 2 3 in
  let actual = CursorPos.create 0 3 in
  let actual = CursorPos.set_row actual sample_multi_line_text 3 in
  CursorPos.equal actual expected

(* set_col *)
let%test "set_col does not clamp if col is in bounds" =
  let expected = CursorPos.create 0 4 in
  let actual = CursorPos.create 0 0 in
  let actual = CursorPos.set_col actual sample_single_line_text 4 in
  CursorPos.equal actual expected

let%test "set_col clamps if col is less than 0" =
  let expected = CursorPos.create 0 0 in
  let actual = CursorPos.create 0 4 in
  let actual = CursorPos.set_col actual sample_single_line_text (-3) in
  CursorPos.equal actual expected

let%test "set_col clamps if col is greater than the number of characters in \
          the line" =
  let expected = CursorPos.create 0 12 in
  let actual = CursorPos.create 0 0 in
  let actual = CursorPos.set_col actual sample_single_line_text 50 in
  CursorPos.equal actual expected

(* set_col_rel forwards *)
let%test "set_col_rel sets the column forward one in bounds" =
  let expected = CursorPos.create 0 5 in
  let actual = CursorPos.create 0 4 in
  let actual = CursorPos.set_col_rel actual sample_single_line_text 1 in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column forward one across line boundary" =
  let expected = CursorPos.create 1 0 in
  let actual = CursorPos.create 0 10 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text 1 in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column forward multiple in bounds" =
  let expected = CursorPos.create 0 5 in
  let actual = CursorPos.create 0 2 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text 3 in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column forward multiple across line boundary" =
  let expected = CursorPos.create 1 1 in
  let actual = CursorPos.create 0 9 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text 3 in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column to the last character when out of bounds"
    =
  let expected = CursorPos.create 0 12 in
  let actual = CursorPos.create 0 0 in
  let actual = CursorPos.set_col_rel actual sample_single_line_text 20 in
  CursorPos.equal actual expected

(* set_col_rel backwards *)

let%test "set_col_rel sets the column backward one in bounds" =
  let expected = CursorPos.create 0 5 in
  let actual = CursorPos.create 0 6 in
  let actual = CursorPos.set_col_rel actual sample_single_line_text (-1) in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column backwards out of bounds" =
  let expected = CursorPos.create 0 0 in
  let actual = CursorPos.create 0 5 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text (-20) in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column backwards one across the line boundary" =
  let expected = CursorPos.create 0 10 in
  let actual = CursorPos.create 1 0 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text (-1) in
  Printf.printf "%s <> %s\n"
    (CursorPos.to_string actual)
    (CursorPos.to_string expected);
  CursorPos.equal actual expected

let%test "set_col_rel sets the column backwards multiple, in bounds" =
  let expected = CursorPos.create 0 2 in
  let actual = CursorPos.create 0 5 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text (-3) in
  CursorPos.equal actual expected

let%test "set_col_rel sets the column backwards multiple, across line boundary"
    =
  let expected = CursorPos.create 0 8 in
  let actual = CursorPos.create 1 2 in
  let actual = CursorPos.set_col_rel actual sample_multi_line_text (-5) in
  CursorPos.equal actual expected

(* set_row_rel *)

let%test "set_row_rel sets the row forwards one, in bounds" =
  let expected = CursorPos.create 1 4 in
  let actual = CursorPos.create 0 4 in
  let actual = CursorPos.set_row_rel actual sample_multi_line_text 1 in
  CursorPos.equal actual expected

let%test "set_row_rel sets the forwards multiple, in bounds" =
  let expected = CursorPos.create 2 4 in
  let actual = CursorPos.create 0 4 in
  let actual = CursorPos.set_row_rel actual sample_multi_line_text 2 in
  CursorPos.equal actual expected

let%test "set_row_rel sets the row backwards one, in bounds" =
  let expected = CursorPos.create 0 4 in
  let actual = CursorPos.create 1 4 in
  let actual = CursorPos.set_row_rel actual sample_multi_line_text (-1) in
  CursorPos.equal actual expected

let%test "set_row_rel sets the row backwards multiple, out of bounds" =
  let expected = CursorPos.create 0 4 in
  let actual = CursorPos.create 2 4 in
  let actual = CursorPos.set_row_rel actual sample_multi_line_text (-2) in
  CursorPos.equal actual expected

let%test "set_row_rel sets the row backwards one, clamped" =
  let expected = CursorPos.create 0 0 in
  let actual = CursorPos.create 0 5 in
  let actual = CursorPos.set_row_rel actual sample_multi_line_text (-1) in
  CursorPos.equal actual expected

let%test "set_row_rel sets the row backwards multiple, clamped" =
  let expected = CursorPos.create 0 0 in
  let actual = CursorPos.create 2 5 in
  let actual = CursorPos.set_row_rel actual sample_multi_line_text (-3) in
  CursorPos.equal actual expected

let%test "set_row_rel sets the column to max when setting row forwards one and \
          column not available" =
  let expected = CursorPos.create 1 2 in
  let actual = CursorPos.create 0 5 in
  let actual = CursorPos.set_row_rel actual sample_multi_column_text 1 in
  CursorPos.equal actual expected

let%test "set_row_rel twice sets the column when setting row forwards over a \
          shorter row" =
  let expected = CursorPos.create 2 4 in
  let actual = CursorPos.create 0 4 in
  let actual = CursorPos.set_row_rel actual sample_multi_column_text 1 in
  let actual = CursorPos.set_row_rel actual sample_multi_column_text 1 in
  CursorPos.equal actual expected

let%test "set_row_rel sets the column to max when setting row forwards one and \
          column not available" =
  let expected = CursorPos.create 1 2 in
  let actual = CursorPos.create 2 5 in
  let actual = CursorPos.set_row_rel actual sample_multi_column_text (-1) in
  CursorPos.equal actual expected

let%test "set_row_rel twice sets the column when setting row backwards over a \
          shorter row" =
  let expected = CursorPos.create 0 5 in
  let actual = CursorPos.create 2 5 in
  let actual = CursorPos.set_row_rel actual sample_multi_column_text (-1) in
  let actual = CursorPos.set_row_rel actual sample_multi_column_text (-1) in
  CursorPos.equal actual expected