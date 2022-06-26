open Tsdl
open Tsdl_ttf
open Helpers

type t = string * int * Ttf.font

let create location size =
  Ttf.open_font location size >>= fun font -> (location, size, font)

let get_width_of_text font text = Ttf.size_utf8 font text >>= fun (w, _) -> w

let create_texture_from_text font ctx fg bg text =
  Ttf.render_utf8_shaded font text (SdlHelpers.of_color fg)
    (SdlHelpers.of_color bg)
  >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface (SdlContext.renderer ctx) surface
  >>= fun texture ->
  Sdl.free_surface surface;
  let texture = Texture.create texture in
  let surface_size = rect_of_sdl surface_size in
  (texture, surface_size)