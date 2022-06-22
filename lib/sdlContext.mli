type t

val create : unit -> t
val destroy : t -> unit
val present : t -> unit
val clear : t -> unit
val set_title : t -> string -> unit
val set_draw_color : t -> Color.t -> unit