type 'a t = { status : int; conn : Conn.t }
(** [Return.t] defines a return type for server commands *)

val ok : Conn.t -> unit t Lwt.t
(** Return an empty "OK" response *)

val err : Conn.t -> string -> 'a t Lwt.t
(** Return an error response *)

val v : Conn.t -> 'a Irmin.Type.t -> 'a -> 'a t Lwt.t
(** Create a new return value *)

val result : Conn.t -> 'a Irmin.Type.t -> 'a Error.result -> 'a t Lwt.t
(** Return a [Result.t] value *)

val finish : 'a Irmin.Type.t -> 'a t -> unit Lwt.t
(** Finalize the return value, this is called from the server's main loop *)
