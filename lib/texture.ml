open Tsdl

type t = Sdl.texture

let create texture = texture
let destroy t = Sdl.destroy_texture t
