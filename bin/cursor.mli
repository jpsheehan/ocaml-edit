(** Represents a cursor that indicates the insertion point inside a document. *)
type cursor

(** Creates a new cursor positioned at the first column of the first line. *)
val create : unit -> cursor

(** Processes the cursor's logic before rendering. *)
val process_hook : cursor -> int -> cursor

(** Renders the cursor to the current render target. *)
val render_hook : cursor -> string list -> Tsdl.Sdl.renderer -> Tsdl_ttf.Ttf.font -> unit

(** Sets the cursor's column relative to the current column. This may cause the cursor to wrap to a new line. *)
val set_column_rel : cursor -> string list -> int -> cursor

(** Sets the cursor's line relative to the current line. This may cause the cursor to wrap to a different column (temporarily). *)
val set_line_rel : cursor -> string list -> int -> cursor

(** Gets the cursor's column. *)
val get_column : cursor -> int

(** Gets the cursor's line. *)
val get_line : cursor -> int
