let ( >>= ) o f =
  match o with
  | Error (`Msg e) -> failwith (Printf.sprintf "Error %s" e)
  | Ok a -> f a

type point = { x : int; y : int }
type size = { w : int; h : int }
