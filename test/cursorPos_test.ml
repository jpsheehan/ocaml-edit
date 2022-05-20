open OEditor

(* create *)

let%test "create creates a CursorPos at the origin" =
  let pos = CursorPos.create 0 0 in
  CursorPos.get_row pos = 0 && CursorPos.get_col pos = 0
