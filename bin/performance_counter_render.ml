open OEditor
open Tsdl
open Tsdl_ttf
open Colors
open OEditor.Helpers

let compute_mean xs =
  match List.length xs with
  | 0 -> 0
  | len ->
      let sum = List.fold_left ( + ) 0 xs in
      sum / len

let compute_max xs =
  match List.length xs with 0 -> 0 | _ -> List.fold_left max 0 xs

let calculate_color value target =
  let color =
    match value with
    | n when n < int_of_float (float_of_int target *. 0.8) -> green
    | n when n < target -> orange
    | _ -> red
  in
  color

let as_bar_graph perfc renderer pixel_format target =
  let px_per_bar = 2 in
  let bar_height = 64 in
  let max_value = Performance_counter.compute perfc compute_max in

  let texture_w = px_per_bar * Performance_counter.length perfc in
  let texture_h = bar_height in
  Sdl.create_texture renderer pixel_format Sdl.Texture.access_target
    ~w:texture_w ~h:texture_h
  >>= fun texture ->
  Sdl.set_render_target renderer (Some texture) >>= fun () ->
  Sdl.set_render_draw_color renderer 0 0 0 0xff >>= fun () ->
  Sdl.render_draw_rect renderer None >>= fun () ->
  for i = 0 to Performance_counter.length perfc - 1 do
    let value = Performance_counter.nth perfc i in
    let x = texture_w - (i * px_per_bar) in
    let y = 0 in
    let w = px_per_bar in
    let h =
      int_of_float
        (float_of_int bar_height *. float_of_int value /. float_of_int max_value)
    in
    let color = calculate_color value target in
    Sdl.set_render_draw_color renderer (Sdl.Color.r color) (Sdl.Color.g color)
      (Sdl.Color.b color) (Sdl.Color.a color)
    >>= fun () ->
    Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h))
    >>= fun () -> ()
  done;
  Sdl.set_render_target renderer None >>= fun () ->
  Sdl.render_copy
    ~dst:
      (Sdl.Rect.create
         ~x:(Sdl.Rect.w (Sdl.render_get_viewport renderer) - texture_w - 10)
         ~y:10 ~w:texture_w ~h:texture_h)
    renderer texture
  >>= fun () -> Sdl.destroy_texture texture

let as_text perfc renderer font target fn =
  let compute_time = Performance_counter.compute perfc fn in
  let compute_time_str = string_of_int compute_time in
  let compute_time_str = String.cat compute_time_str " ms" in

  let color = calculate_color compute_time target in

  Ttf.render_text_blended font compute_time_str color >>= fun surface ->
  let surface_w, surface_h = Sdl.get_surface_size surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.render_copy
    ~dst:
      (Sdl.Rect.create
         ~x:(Sdl.Rect.w (Sdl.render_get_viewport renderer) - surface_w - 10)
         ~y:10 ~w:surface_w ~h:surface_h)
    renderer texture
  >>= fun () ->
  Sdl.free_surface surface;
  Sdl.destroy_texture texture

let as_mean_text perfc renderer font target =
  as_text perfc renderer font target compute_mean

let as_max_text perfc renderer font target =
  as_text perfc renderer font target compute_max
