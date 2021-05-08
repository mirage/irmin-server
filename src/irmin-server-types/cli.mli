val uri : Uri.t option Cmdliner.Term.t

val default_uri : Uri.t

val store :
  (string option * Irmin_unix.Resolver.hash option * string option)
  Cmdliner.Term.t

val setup_log : unit Cmdliner.Term.t
