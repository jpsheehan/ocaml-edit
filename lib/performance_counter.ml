open Helpers

type 'a performance_counter = { items : 'a list; size : int }

let create size = { items = []; size }

let push perfc item =
  if List.length perfc.items + 1 <= perfc.size then
    { perfc with items = item :: perfc.items }
  else { perfc with items = item :: init perfc.items }

let compute perfc fn = fn perfc.items
let clear perfc = { perfc with items = [] }
let length perfc = List.length perfc.items
let nth perfc idx = List.nth perfc.items idx