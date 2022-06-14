open Tsdl
open Tsdl_ttf
open Helpers

type t = {
  text : DocText.t;
  cache : (Sdl.texture * Sdl.rect) option list;
  selection_cache : (Sdl.texture * Sdl.rect) option list;
}

let create_from_docText text =
  {
    text;
    cache = List.init (DocText.get_number_of_lines text) (fun _ -> None);
    selection_cache =
      List.init (DocText.get_number_of_lines text) (fun _ -> None);
  }

let mark_line_as_dirty textCache row =
  (match List.nth textCache.cache row with
  | Some (texture, _) -> Sdl.destroy_texture texture
  | None -> ());
  (match List.nth textCache.selection_cache row with
  | Some (texture, _) -> Sdl.destroy_texture texture
  | None -> ());
  {
    textCache with
    cache = replace textCache.cache row None;
    selection_cache = replace textCache.selection_cache row None;
  }

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
  let selection_cache = remove textCache.selection_cache row in
  let text = DocText.remove_line textCache.text row in
  { cache; text; selection_cache }

let insert_line_after textCache row line =
  let cache = insert_after textCache.cache row None in
  let selection_cache = insert_after textCache.selection_cache row None in
  let text = DocText.insert_line_after textCache.text row line in
  { text; cache; selection_cache }

let flush_textures textCache =
  mark_lines_as_dirty textCache
    (range ~min:0 ~max:(List.length textCache.cache))

let create_texture_from_text renderer font text fg bg : Sdl.texture * Sdl.rect =
  Ttf.render_utf8_shaded font text fg bg >>= fun surface ->
  let surface_size = Sdl.get_clip_rect surface in
  Sdl.create_texture_from_surface renderer surface >>= fun texture ->
  Sdl.free_surface surface;
  (texture, surface_size)

(* let create_selection_texture_from_text renderer font text cursor fg bg idx :
     Sdl.texture * Sdl.rect =
   () *)

let set_texture textCache renderer font _cursor fg bg idx =
  if get_width_of_text font (get_line textCache idx) = 0 then textCache
  else
    let texture, size =
      create_texture_from_text renderer font
        (DocText.get_line textCache.text idx)
        fg bg
    in
    let cache = replace textCache.cache idx (Some (texture, size)) in
    (* match Cursor.get_selection cursor with
       | Some (cursor_a, cursor_b) ->
           let selection_texture, selection_size =
             create_selection_texture_from_text renderer font
               (DocText.get_line textCache.text idx)
               (cursor_a, cursor_b) fg bg idx
           in
           let selection_cache =
             replace textCache.selection_cache idx
               (Some (selection_texture, selection_size))
           in
           { textCache with cache; selection_cache } *)
    (* | None -> *)
    { textCache with cache }

let prepare_textures textCache renderer font cursor fg bg first_line last_line =
  let last_line_exclusive =
    min (last_line + 1) (get_number_of_lines textCache)
  in
  let first_line = max first_line 0 in
  let idxs = range ~min:first_line ~max:last_line_exclusive in
  let idxs =
    List.filter (fun idx -> List.nth textCache.cache idx = None) idxs
  in
  List.fold_left
    (fun textCache idx -> set_texture textCache renderer font cursor fg bg idx)
    textCache idxs

let get_texture textCache idx =
  match List.nth textCache.cache idx with
  | None ->
      None (*failwith (Printf.sprintf "Texture for line %d is not ready" idx)*)
  | Some (texture, size) -> Some (texture, size)

let get_selection_texture textCache idx = List.nth textCache.selection_cache idx
let get_text textCache = textCache.text
