module type S = sig
  type t

  module Store : Irmin_pack_layered.S

  module Command : Command.S with module Store = Store

  val v :
    ?tls_config:[ `Cert_file of string ] * [ `Key_file of string ] ->
    uri:string ->
    Irmin.config ->
    t Lwt.t

  val serve : ?http:int -> t -> unit Lwt.t

  val commands : (string, Command.t) Hashtbl.t
end

module type Server = sig
  module type S = S

  module Make (C : Command.S) : S with module Store = C.Store
end
