val uri : string Cmdliner.Term.t

val log_level : Logs.level Cmdliner.Term.t

val contents : string option Cmdliner.Term.t

val hash : Irmin_unix.Resolver.hash option Cmdliner.Term.t

val default_contents : string

val default_hash : Irmin_unix.Resolver.hash
