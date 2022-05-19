open OEditor.Helpers

let sample_list = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

(* Clamp *)
let%test "clamp" = Int.equal (clamp 4 0 10) 4
let%test "clamp min" = Int.equal (clamp 4 6 10) 6
let%test "clamp max" = Int.equal (clamp 11 6 10) 10
(* Take *)
let%test "take from empty list" = List.equal Int.equal (take [] 5) []

let%test "take zero elements from list" =
  List.equal Int.equal (take sample_list 0) []

let%test "take from list" =
  List.equal Int.equal (take sample_list 3) [ 1; 2; 3 ]

let%test "take more elements than the list contains" =
  List.equal Int.equal (take sample_list 20) sample_list

let%test "take a negative number of elements" =
  List.equal Int.equal (take sample_list (-4)) []

(* Skip *)
let%test "skip zero elements" =
  List.equal Int.equal (skip sample_list 0) sample_list

let%test "skip elements in an empty list" = List.equal Int.equal (skip [] 5) []

let%test "skip some elements" =
  List.equal Int.equal (skip sample_list 3) [ 4; 5; 6; 7; 8; 9; 10 ]

let%test "skip negative elements" =
  List.equal Int.equal (skip sample_list (-2)) sample_list

let%test "skip more elements than the list contains" =
  List.equal Int.equal (skip sample_list 22) []

(* Replace *)

let%test "replace an element in a list" =
  List.equal Int.equal (replace sample_list 4 42)
    [ 1; 2; 3; 4; 42; 6; 7; 8; 9; 10 ]

let%test "replace an element outside of the bounds of the list" =
  List.equal Int.equal (replace sample_list 42 99) sample_list

let%test "replace an element with a negative index" =
  List.equal Int.equal (replace sample_list (-42) 99) sample_list

let%test "replace an element in an empty list" =
  List.equal Int.equal (replace [] 0 42) []

(* remove *)
let%test "remove an element in a list" =
  List.equal Int.equal (remove sample_list 0) [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

let%test "remove an element at a negative index" =
  List.equal Int.equal (remove sample_list (-5)) sample_list

let%test "remove an element at an index greater than the number of elements \
          held" =
  List.equal Int.equal (remove sample_list 42) sample_list

let%test "remove an element from an empty list" =
  List.equal Int.equal (remove [] 0) []

(* Insert after *)
let%test "insert_after inserts an element into a list" =
  List.equal Int.equal
    (insert_after sample_list 0 42)
    [ 1; 42; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

let%test "insert_after inserts an element into an empty list" =
  List.equal Int.equal (insert_after [] 0 99) [ 99 ]
