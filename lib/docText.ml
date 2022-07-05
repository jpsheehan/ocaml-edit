type t = { lines : string list }

let create () = { lines = [ "" ] }

let create_from_string str =
  let text = { lines = String.split_on_char '\n' str } in
  if List.length text.lines = 0 then create () else text

let get_number_of_lines t = List.length t.lines
let get_line t idx = List.nth t.lines idx
let replace_line t idx line = { lines = Helpers.replace t.lines idx line }
let remove_line t idx = { lines = Helpers.remove t.lines idx }

let insert_line_after t idx line =
  { lines = Helpers.insert_after t.lines idx line }

let create_from_file filename =
  let file = open_in filename in
  let rec read_lines_from_file lines =
    try
      let line = input_line file in
      read_lines_from_file (line :: lines)
    with End_of_file ->
      close_in file;
      lines
  in
  let text = { lines = List.rev (read_lines_from_file []) } in
  if List.length text.lines = 0 then create () else text
