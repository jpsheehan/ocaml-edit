let ( >>= ) o f =
  match o with
  | Error (`Msg e) -> failwith (Printf.sprintf "Error %s" e)
  | Ok a -> f a
