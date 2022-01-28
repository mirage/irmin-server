val uri : Uri.t option Cmdliner.Term.t
(** Generic URI term *)

val default_uri : Uri.t
(** Default URI for command line applications *)

val config_path : string option Cmdliner.Term.t
(** Command line argument to specify configuration path *)

val setup_log : unit Cmdliner.Term.t
(** Term to initialize logging *)

val codec : (module Conn.Codec.S) Cmdliner.Term.t
