open OEditor.Helpers

let%test "clamp" = Int.equal (clamp 4 0 10) 4
let%test "clamp" = Int.equal (clamp 4 6 10) 6