type cursor
(** Represents a cursor that indicates the insertion point inside a document. *)

val create : unit -> cursor
(** Creates a new cursor positioned at the first column of the first line. *)

val process_hook : cursor -> int -> cursor
(** Processes the cursor's logic before rendering. *)

val render_hook :
  cursor ->
  DocText.t ->
  Helpers.point ->
  Tsdl.Sdl.renderer ->
  Tsdl_ttf.Ttf.font ->
  unit
(** Renders the cursor to the current render target. *)

val set_column : cursor -> DocText.t -> int -> cursor
(** Sets the cursor's column to an absolute value. *)

val set_column_rel : cursor -> DocText.t -> int -> cursor
(** Sets the cursor's column relative to the current column. This may cause the cursor to wrap to a new line. *)

val set_line : cursor -> DocText.t -> int -> cursor
(** Sets the cursor's line to an absolute value. *)

val set_line_rel : cursor -> DocText.t -> int -> cursor
(** Sets the cursor's line relative to the current line. This may cause the cursor to wrap to a different column (temporarily). *)

val get_column : cursor -> int
(** Gets the cursor's column or secondary column (if it exists). *)

val get_line : cursor -> int
(** Gets the cursor's line or secondary line (if it exists). *)

val is_dirty : cursor -> bool
val postrender_hook : cursor -> cursor
val set_selection_end : cursor -> DocText.t -> CursorPos.t -> cursor
val set_selection_end_rel : cursor -> DocText.t -> CursorPos.t -> cursor
val select_none : cursor -> cursor
val select_all : cursor -> DocText.t -> cursor
val has_selection : cursor -> bool
val get_selection : cursor -> (CursorPos.t * CursorPos.t) option