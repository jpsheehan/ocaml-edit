let ( >>= ) o f =
  match o with
  | Error (`Msg e) -> failwith (Printf.sprintf "Error %s" e)
  | Ok a -> f a

let clamp x min max = if x < min then min else if x > max then max else x

type point = { x : int; y : int }
type size = { w : int; h : int }

let take list n =
  let l = ref [] in
  for i = 0 to n - 1 do
    l <- (List.nth list i) :: !l;
  done
  !l