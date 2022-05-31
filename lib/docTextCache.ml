open Tsdl
open Tsdl_ttf
open Helpers

type t = { text : DocText.t; cache : (Sdl.texture * Sdl.rect) option list }

let create_from_docText text =
  { text; cache = List.init (DocText.get_number_of_lines text) (fun _ -> None) }

let mark_line_as_dirty textCache row =
  (match List.nth textCache.cache row with
  | Some (texture, _) -> Sdl.destroy_texture texture
  | None -> ());
  { textCache with cache = replace textCache.cache row None }

let mark_lines_as_dirty textCache rows =
  List.fold_left mark_line_as_dirty textCache rows

let create () =
  let text = DocText.create () in
  create_from_docText text

let create_from_string s =
  let text = DocText.create_from_string s in
  create_from_docText text

let create_from_file filename =
  let text = DocText.create_from_file filename in
  create_from_docText text

let get_number_of_lines textCache = DocText.get_number_of_lines textCache.text
let get_line textCache row = DocText.get_line textCache.text row

let replace_line textCache row line =
  let textCache = mark_line_as_dirty textCache row in
  { textCache with text = DocText.replace_line textCache.text row line }

let remove_line textCache row =
  let textCache = mark_line_as_dirty textCache row in
  let cache = remove textCache.cache row in
  let text = DocText.remove_line textCache.text row in
  { cache; text }

let insert_line_after textCache row line =
  let cache = insert_after textCache.cache row None in
  let text = DocText.insert_line_after textCache.text row line in
  { text; cache }

let flush_textures textCache =
  mark_lines_as_dirty textCache
    (range ~min:0 ~max:(List.length textCache.cache))

let create_texture_from_text renderer font text fg bg : Sdl.texture * Sdl.rect =
  Ttf.render_utf8_shaded font text fg bg >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  (texture, surface_size)

let set_texture textCache renderer font fg bg idx =
  if get_width_of_text font (get_line textCache idx) = 0 then textCache
  else (
    Printf.printf "setting texture for line %d\n" idx;
    let texture, size =
      create_texture_from_text renderer font
        (DocText.get_line textCache.text idx)
        fg bg
    in
    {
      textCache with
      cache = replace textCache.cache idx (Some (texture, size));
    })

let prepare_textures textCache renderer font fg bg first_line last_line =
  let idxs = range ~min:first_line ~max:(last_line + 1) in
  let idxs =
    List.filter (fun idx -> List.nth textCache.cache idx = None) idxs
  in
  List.fold_left
    (fun textCache idx -> set_texture textCache renderer font fg bg idx)
    textCache idxs

let get_texture textCache idx =
  match List.nth textCache.cache idx with
  | None ->
      None (*failwith (Printf.sprintf "Texture for line %d is not ready" idx)*)
  | Some (texture, size) -> Some (texture, size)

let get_text textCache = textCache.text
