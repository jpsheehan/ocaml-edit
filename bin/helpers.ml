let ( >>= ) o f =
  match o with
  | Error (`Msg e) -> failwith (Printf.sprintf "Error %s" e)
  | Ok a -> f a

let clamp x min max = if x < min then min else if x > max then max else x

type point = { x : int; y : int }
type size = { w : int; h : int }

let take list n =
  let rec aux list n taken =
    if n <= 0 then taken
    else match list with [] -> taken | h :: t -> aux t (n - 1) (h :: taken)
  in
  List.rev (aux list n [])

let skip list n =
  let rec aux list n =
    if n <= 0 then list else match list with [] -> [] | _ :: t -> aux t (n - 1)
  in
  aux list n

let replace list n item = take list n @ (item :: skip list (n + 1))
let insert_after list n item = take list (n + 1) @ (item :: skip list (n + 1))
let remove list n = take list n @ skip list (n + 1)

let split_string_at str n =
  let before = String.sub str 0 n in
  let after = String.sub str n (String.length str - n) in
  (before, after)

let init l = take l (List.length l - 1)

let last l =
  match skip l (List.length l - 1) with [] -> None | h :: _ -> Some h
