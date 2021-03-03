type 'a t = { n_items : int; mutable index : int; conn : Conn.t }

val make : int -> Conn.t -> 'a t Lwt.t

val ok : Conn.t -> unit t Lwt.t

val err : Conn.t -> string -> 'a t Lwt.t

val write : 'a Irmin.Type.t -> 'a -> 'a t -> 'a t Lwt.t

val check : 'a t -> int -> unit

val v : Conn.t -> 'a Irmin.Type.t -> 'a -> 'a t Lwt.t

val flush : 'a t -> unit Lwt.t
