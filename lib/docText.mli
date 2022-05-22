type t

val create : unit -> t
val create_from_string : string -> t
val create_from_file : string -> t
val get_number_of_lines : t -> int
val get_line : t -> int -> string
val replace_line : t -> int -> string -> t
val remove_line : t -> int -> t
val insert_line_after : t -> int -> string -> t