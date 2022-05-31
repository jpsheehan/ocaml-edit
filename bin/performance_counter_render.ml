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

let as_mean_text perfc renderer font target =
  let compute_time = Performance_counter.compute perfc compute_mean in
  let compute_time_str = string_of_int compute_time in
  let compute_time_str = String.cat compute_time_str " ms" in

  let color =
    match compute_time with
    | n when n < int_of_float (float_of_int target *. 0.8) -> green
    | n when n < target -> orange
    | _ -> red
  in

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
