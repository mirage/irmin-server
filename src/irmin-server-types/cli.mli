val uri : Uri.t option Cmdliner.Term.t
(** Generic URI term *)

val default_uri : Uri.t
(** Default URI for command line applications *)

val config_path : string option Cmdliner.Term.t
(** Command line argument to specify configuration path *)

val store :
  (string option * Irmin_unix.Resolver.hash option * string option)
  Cmdliner.Term.t
(** Store term *)

val setup_log : unit Cmdliner.Term.t
(** Term to initialize logging *)
