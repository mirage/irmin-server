type t = { n_items : int; mutable index : int; conn : Conn.t }

val make : int -> Conn.t -> t Lwt.t

val ok : Conn.t -> t Lwt.t

val err : Conn.t -> string -> t Lwt.t

val write : 'a Irmin.Type.t -> 'a -> t -> t Lwt.t

val check : t -> int -> unit

val v : Conn.t -> 'a Irmin.Type.t -> 'a -> t Lwt.t

val flush : t -> unit Lwt.t
