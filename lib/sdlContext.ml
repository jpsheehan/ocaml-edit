open Tsdl
open Tsdl_ttf
open Helpers

type t = { window : Sdl.window; renderer : Sdl.renderer; title : string }

let of_color c =
  Sdl.Color.create ~r:(Color.r c) ~g:(Color.g c) ~b:(Color.b c) ~a:(Color.a c)

let create title =
  Sdl.init Sdl.Init.video >>= fun () ->
  Ttf.init () >>= fun () ->
  Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.(shown + resizable)
  >>= fun (window, renderer) ->
  Sdl.set_window_title window title;
  { window; renderer; title }

let destroy t =
  Sdl.destroy_renderer t.renderer;
  Sdl.destroy_window t.window

let present t = Sdl.render_present t.renderer
let clear t = Sdl.render_clear t.renderer >>= fun () -> ()
let delay _t ms = Int32.of_int ms |> Sdl.delay

let set_draw_color t color =
  Sdl.set_render_draw_color t.renderer (Color.r color) (Color.g color)
    (Color.b color) (Color.a color)
  >>= fun () -> ()

let set_target t tgt = Sdl.set_render_target t.renderer tgt >>= fun () -> ()
let fill_rect t r = Sdl.render_fill_rect t.renderer r >>= fun () -> ()