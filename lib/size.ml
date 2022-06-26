type t = int * int

let create w h = (w, h)
let w (w, _) = w
let h (_, h) = h