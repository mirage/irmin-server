val uri : string Cmdliner.Term.t

val contents : string option Cmdliner.Term.t

val hash : Irmin_unix.Resolver.hash option Cmdliner.Term.t

val store :
  (string option * Irmin_unix.Resolver.hash option * string option)
  Cmdliner.Term.t

val setup_log : unit Cmdliner.Term.t
