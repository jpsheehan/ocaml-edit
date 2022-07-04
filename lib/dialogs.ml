type file_selection = {
  filename : string option;
  multiple : bool;
  directory : bool;
  save : bool;
  separator : string option;
}

type question = { text : string }
type dialog_options = FileSelection of file_selection | Question of question

type zenity_options = {
  title : string option;
  window_icon : string option;
  width : int option;
  height : int option;
  timeout : int option;
  dialog : dialog_options;
}

let get_arguments_for_file_selection_dialog options =
  let args = ref [ "--file-selection" ] in
  (match options.filename with
  | Some filename -> args := ("--filename=" ^ filename) :: !args
  | _ -> ());
  (match options.separator with
  | Some separator -> args := ("--separator=" ^ separator) :: !args
  | _ -> ());
  if options.multiple then args := "--multiple" :: !args;
  if options.directory then args := "--directory" :: !args;
  if options.save then args := "--save" :: !args;

  !args

let get_arguments_for_question_dialog options =
  let args = ref [ "--question" ] in
  args := ("--text=" ^ options.text) :: !args;
  !args

let get_arguments_for_dialog_options options =
  match options with
  | FileSelection options -> get_arguments_for_file_selection_dialog options
  | Question options -> get_arguments_for_question_dialog options

let execute_zenity options =
  let args = ref [] in
  (match options.title with
  | Some title -> args := ("--title=" ^ title) :: !args
  | _ -> ());
  (match options.window_icon with
  | Some window_icon -> args := ("--window-icon=" ^ window_icon) :: !args
  | _ -> ());
  (match options.width with
  | Some width -> args := ("--width=" ^ string_of_int width) :: !args
  | _ -> ());
  (match options.height with
  | Some height -> args := ("--height=" ^ string_of_int height) :: !args
  | _ -> ());
  (match options.timeout with
  | Some timeout -> args := ("--timeout=" ^ string_of_int timeout) :: !args
  | _ -> ());
  args := get_arguments_for_dialog_options options.dialog @ !args;

  let command = Filename.quote_command "zenity" !args in
  let input = Unix.open_process_in command in

  match Unix.waitpid [] (-1) with
  | _, Unix.WEXITED _ -> (
      try Ok [ input_line input ] with End_of_file -> Ok [])
  | 1, _ -> Error "User pressed cancel"
  | -1, _ -> Error "An unexpected error occurred"
  | 5, _ -> Error "Timeout occurred"
  | x, _ -> Error (Printf.sprintf "Zenity exited with '%d' code." x)

let question title prompt =
  match
    execute_zenity
      {
        title = Some title;
        width = None;
        height = None;
        timeout = None;
        window_icon = None;
        dialog = Question { text = prompt };
      }
  with
  | Ok _ -> true
  | _ -> false

let open_file title =
  match
    execute_zenity
      {
        title = Some title;
        width = None;
        height = None;
        window_icon = None;
        timeout = None;
        dialog =
          FileSelection
            {
              multiple = false;
              filename = None;
              directory = false;
              save = false;
              separator = None;
            };
      }
  with
  | Ok [] -> None
  | Ok filenames -> Some (List.nth filenames 0)
  | Error msg -> failwith msg

let save_file title =
  match
    execute_zenity
      {
        title = Some title;
        width = None;
        height = None;
        window_icon = None;
        timeout = None;
        dialog =
          FileSelection
            {
              multiple = false;
              filename = None;
              directory = false;
              save = true;
              separator = None;
            };
      }
  with
  | Ok [] -> None
  | Ok filenames -> Some (List.nth filenames 0)
  | Error msg -> failwith msg
