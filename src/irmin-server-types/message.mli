val encode : 'a Irmin.Type.t -> 'a -> string
(** Encode a message, this is called inside of [Conn.write_message] *)

val decode : 'a Irmin.Type.t -> string -> ('a, [ `Msg of string ]) result
(** Decode a message, this is called inside of [Conn.read_message] *)

val write_raw : Conduit_lwt_unix.oc -> string -> unit Lwt.t
(** Write data to an output channel *)

val write : Conduit_lwt_unix.oc -> 'a Irmin.Type.t -> 'a -> unit Lwt.t
(** Write a typed message containing a value to an output channel *)

val read_raw : buffer:bytes -> Conduit_lwt_unix.ic -> bytes Lwt.t
(** Read data from input channel, [buffer] will be used if the incoming message
    data will fit *)

val read :
  buffer:bytes ->
  Conduit_lwt_unix.ic ->
  'a Irmin.Type.t ->
  ('a, [ `Msg of string ]) result Lwt.t
(** Read typed message from an input channel *)
