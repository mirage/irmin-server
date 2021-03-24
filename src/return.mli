type 'a t = { status : int; conn : Conn.t }

val ok : Conn.t -> unit t Lwt.t

val err : Conn.t -> string -> 'a t Lwt.t

val v : Conn.t -> 'a Irmin.Type.t -> 'a -> 'a t Lwt.t

val result : Conn.t -> 'a Irmin.Type.t -> 'a Error.result -> 'a t Lwt.t
