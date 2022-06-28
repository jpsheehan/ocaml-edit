open Tsdl
open Tsdl_ttf
open Helpers

let rect_of_sdl r =
  Rect.create ~x:(Sdl.Rect.x r) ~y:(Sdl.Rect.y r) ~w:(Sdl.Rect.w r)
    ~h:(Sdl.Rect.h r)

let sdl_of_rect r =
  Sdl.Rect.create ~x:(Rect.x r) ~y:(Rect.y r) ~w:(Rect.w r) ~h:(Rect.h r)

type t = { window : Sdl.window; renderer : Sdl.renderer; title : string }

let of_color c =
  Sdl.Color.create ~r:(Color.r c) ~g:(Color.g c) ~b:(Color.b c) ~a:(Color.a c)

let copy t texture ~src ~dst =
  let src = sdl_of_rect src in
  let dst = sdl_of_rect dst in
  Sdl.render_copy t.renderer texture ~src ~dst >>= fun () -> ()

let get_pixel_format ctx = Sdl.get_window_pixel_format ctx.window

let create title =
  Sdl.init Sdl.Init.video >>= fun () ->
  Ttf.init () >>= fun () ->
  Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.(shown + resizable)
  >>= fun (window, renderer) ->
  Sdl.set_window_title window title;
  { window; renderer; title }

let destroy t =
  Sdl.destroy_renderer t.renderer;
  Sdl.destroy_window t.window;
  Ttf.quit ();
  Sdl.quit ()

let set_title t title = Sdl.set_window_title t.window title
let present t = Sdl.render_present t.renderer
let clear t = Sdl.render_clear t.renderer >>= fun () -> ()
let delay _t ms = Int32.of_int ms |> Sdl.delay

let set_draw_color t color =
  Sdl.set_render_draw_color t.renderer (Color.r color) (Color.g color)
    (Color.b color) (Color.a color)
  >>= fun () -> ()

let set_draw_blend_mode t mode =
  Sdl.set_render_draw_blend_mode t.renderer
    (if Blend.get mode = 0 then Sdl.Blend.mode_none else Sdl.Blend.mode_add)
  >>= fun () -> ()

let fill_rect t = function
  | Some rect ->
      let rect = sdl_of_rect rect in
      Sdl.render_fill_rect t.renderer (Some rect) >>= fun () -> ()
  | None -> Sdl.render_fill_rect t.renderer None >>= fun () -> ()

let get_ticks () = Int32.to_int (Sdl.get_ticks ())

let draw_line t x1 y1 x2 y2 =
  Sdl.render_draw_line t.renderer x1 y1 x2 y2 >>= fun () -> ()

let set_target t tgt = Sdl.set_render_target t.renderer tgt >>= fun () -> ()
let get_rect t = rect_of_sdl (Sdl.render_get_clip_rect t.renderer)

type font = string * int * Ttf.font

let font_create location size =
  Ttf.open_font location size >>= fun font -> (location, size, font)

let font_get_width_of_text (_, _, font) text =
  Ttf.size_utf8 font text >>= fun (w, _) -> w

let font_create_texture_from_text (_, _, font) ctx fg bg text =
  Ttf.render_utf8_shaded font text (of_color fg) (of_color bg)
  >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface ctx.renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  let surface_size = rect_of_sdl surface_size in
  (texture, surface_size)

let font_height (_, _, font) = Ttf.font_height font

let font_size_utf8 (_, _, font) text =
  Ttf.size_utf8 font text >>= fun (w, h) -> (w, h)

type texture = Sdl.texture

let texture_create ctx ~w ~h =
  Sdl.create_texture ctx.renderer (get_pixel_format ctx)
    Sdl.Texture.access_target ~w ~h
  >>= fun texture -> texture

let texture_destroy t = Sdl.destroy_texture t