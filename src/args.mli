type 'a t

val v : mode:([< `Read | `Write ] as 'a) -> count:int -> Conn.t -> 'a t

val next : [ `Read ] t -> 'a Irmin.Type.t -> ('a, Error.t) result Lwt.t

val next_raw : [ `Read ] t -> bytes Lwt.t

val remaining : [< `Read | `Write ] t -> int

val write : [ `Write ] t -> 'a Irmin.Type.t -> 'a -> unit Lwt.t

val raise_error : [< `Read | `Write ] t -> string -> 'a
