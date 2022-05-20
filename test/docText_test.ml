open OEditor

let sample_string = "Hello, World!"
let sample_other_string = "Another line bites the dust"
let sample_multiline_string = sample_string ^ "\n" ^ sample_other_string

(* create *)

let%test "create creates an empty DocText" =
  String.equal
    (let dt = Doctext.create () in
     Doctext.get_line dt 0)
    ""

(* create_from_string *)

let%test "create_from_string works with an empty string" =
  String.equal
    (let dt = Doctext.create_from_string "" in
     Doctext.get_line dt 0)
    ""

let%test "create_from_string works with single line" =
  String.equal
    (let dt = Doctext.create_from_string sample_string in
     Doctext.get_line dt 0)
    sample_string

let%test "create_from_string works with multiple lines" =
  String.equal
    (let dt = Doctext.create_from_string sample_multiline_string in
     Doctext.get_line dt 1)
    sample_other_string

let%test "create_from_string works with leading newline" =
  String.equal
    (let dt = Doctext.create_from_string ("\n" ^ sample_string) in
     Doctext.get_line dt 0)
    ""

let%test "create_from_string works with trailing newline" =
  String.equal
    (let dt = Doctext.create_from_string (sample_other_string ^ "\n") in
     Doctext.get_line dt 1)
    ""

(* get_number_of_lines *)
let%test "get_number_of_lines works for empty document" =
  Int.equal
    (let dt = Doctext.create () in
     Doctext.get_number_of_lines dt)
    1

let%test "get_number_of_lines works with multiline document" =
  Int.equal
    (let dt = Doctext.create_from_string sample_multiline_string in
     Doctext.get_number_of_lines dt)
    2
