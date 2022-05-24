type t

val create : int -> int -> t
val get_col : t -> int
val get_row : t -> int

val set_row : t -> DocText.t -> int -> t
(** Sets the row to a particular value. *)

val set_col : t -> DocText.t -> int -> t
(** Sets the column to a particular value. *)

val set_row_rel : t -> DocText.t -> int -> t
(** Sets the row relative to where it is. *)

val set_col_rel : t -> DocText.t -> int -> t
(** Sets the column relative to where it is. *)

val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : t -> string