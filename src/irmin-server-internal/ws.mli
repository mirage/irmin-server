(** Useful functions for the Websocket implementation. *)

val utf16_to_utf8 : string -> string
(** Converts from utf16 to utf8 replacing "\000" with [u_rep]. *)

val utf8_to_utf16 : string -> string
(** Converts from utf8 to utf16 interpreting [u_rep] as "\000". *)
