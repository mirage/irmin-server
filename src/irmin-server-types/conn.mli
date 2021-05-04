type t = {
  flow : Conduit_lwt_unix.flow;
  ic : Conduit_lwt_unix.ic;
  oc : Conduit_lwt_unix.oc;
  buffer : bytes;
}

val v :
  ?buffer_size:int ->
  Conduit_lwt_unix.flow ->
  Conduit_lwt_unix.ic ->
  Conduit_lwt_unix.oc ->
  t

val read_message : t -> 'a Irmin.Type.t -> 'a Error.result Lwt.t

val write_message : t -> 'a Irmin.Type.t -> 'a -> unit Lwt.t

val ok : t -> unit Lwt.t

val err : t -> string -> unit Lwt.t
