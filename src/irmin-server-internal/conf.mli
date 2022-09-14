val spec : Irmin.Backend.Conf.Spec.t

module Key : sig
  val uri : Uri.t Irmin.Backend.Conf.key
  val hostname : string Irmin.Backend.Conf.key
  val tls : bool Irmin.Backend.Conf.key
end

val v : ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config

