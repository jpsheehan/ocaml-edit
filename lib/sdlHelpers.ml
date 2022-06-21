open Tsdl

let of_color c =
  Sdl.Color.create ~r:(Color.r c) ~g:(Color.g c) ~b:(Color.b c) ~a:(Color.a c)
