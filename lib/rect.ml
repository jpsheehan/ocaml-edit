type t = int * int * int * int

let create x y w h = (x, y, w, h)
let x (x, _, _, _) = x
let y (_, y, _, _) = y
let w (_, _, w, _) = w
let h (_, _, _, h) = h