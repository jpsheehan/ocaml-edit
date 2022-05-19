type t

val create : int -> int -> t
val get_col : t -> int
val get_row : t -> int

val set_row : t -> Doctext.t -> int -> bool -> t
(** Sets the row to a particular value. *)

val set_col : t -> Doctext.t -> int -> bool -> t
(** Sets the column to a particular value. *)

val set_row_rel : t -> Doctext.t -> int -> bool -> t
(** Sets the row relative to where it is. *)

val set_col_rel : t -> Doctext.t -> int -> bool -> t
(** Sets the column relative to where it is. *)

val compare : t -> t -> int