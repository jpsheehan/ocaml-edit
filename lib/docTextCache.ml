open Tsdl
open Tsdl_ttf
open Helpers

type docTextCache = { text : DocText.t; cache : Sdl.texture option list }

let create_from_docText text =
  { text; cache = List.init (DocText.get_number_of_lines text) (fun _ -> None) }

let mark_line_as_dirty textCache row =
  (match List.nth textCache.cache row with
  | Some texture -> Sdl.destroy_texture texture
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

let destroy textCache =
  mark_lines_as_dirty textCache
    (range ~min:0 ~max:(List.length textCache.cache))

let create_texture_from_text renderer font text bg : Sdl.texture * Sdl.rect =
  let fg = Sdl.Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff in
  Ttf.render_utf8_shaded font text fg bg >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  (texture, surface_size)

let set_texture textCache renderer font bg idx =
  let texture, _ =
    create_texture_from_text renderer font
      (DocText.get_line textCache.text idx)
      bg
  in
  { textCache with cache = replace textCache.cache idx (Some texture) }

let prepare_textures textCache renderer font bg first_line last_line =
  let idxs = range ~min:first_line ~max:(last_line + 1) in
  let idxs =
    List.filter (fun idx -> List.nth textCache.cache idx = None) idxs
  in
  List.fold_left
    (fun textCache idx -> set_texture textCache renderer font bg idx)
    textCache idxs

let get_texture textCache idx =
  match List.nth textCache.cache idx with
  | None -> failwith "Texture is not ready"
  | Some texture -> texture
