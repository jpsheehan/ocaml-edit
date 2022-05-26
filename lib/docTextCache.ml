open Tsdl
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
  mark_lines_as_dirty textCache (0 to 2)