type t

val v : count:int -> Conn.t -> t

val next : t -> 'a Irmin.Type.t -> ('a, Error.t) result Lwt.t

val next_raw : t -> bytes Lwt.t

val remaining : t -> int

val write : t -> 'a Irmin.Type.t -> 'a -> unit Lwt.t

val raise_error : t -> string -> 'a
