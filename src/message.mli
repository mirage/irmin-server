val write_raw : Conduit_lwt_unix.oc -> string -> unit Lwt.t

val write : Conduit_lwt_unix.oc -> 'a Irmin.Type.t -> 'a -> unit Lwt.t

val read_raw : Conduit_lwt_unix.ic -> bytes Lwt.t

val read :
  Conduit_lwt_unix.ic ->
  'a Irmin.Type.t ->
  ('a, [ `Msg of string ]) result Lwt.t
