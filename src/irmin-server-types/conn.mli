type t = {
  flow : Conduit_lwt_unix.flow;
  ic : Conduit_lwt_unix.ic;
  oc : Conduit_lwt_unix.oc;
  buffer : bytes;
}
(** Basic connection type, used by both clients and servers *)

val v :
  ?buffer_size:int ->
  Conduit_lwt_unix.flow ->
  Conduit_lwt_unix.ic ->
  Conduit_lwt_unix.oc ->
  t
(** Create a new connection using [flow], [ic] and [oc] from conduit *)

val read_message : t -> 'a Irmin.Type.t -> 'a Error.result Lwt.t
(** Read the next message *)

val write_message : t -> 'a Irmin.Type.t -> 'a -> unit Lwt.t
(** Write a message *)

val ok : t -> unit Lwt.t
(** Send "OK" message with [unit] response *)

val err : t -> string -> unit Lwt.t
(** Send error message *)
