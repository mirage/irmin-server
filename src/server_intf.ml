module type S = sig
  type t

  module Store : Irmin_pack_layered.S with type key = string list

  module Command : Command.S with module Store = Store

  val v :
    ?tls_config:[ `Cert_file of string ] * [ `Key_file of string ] ->
    uri:string ->
    Irmin.config ->
    t Lwt.t
  (** Create an instance of the server *)

  val serve : ?graphql:int -> t -> unit Lwt.t
  (** Run the server, optionally enabling the GraphQL server as well *)

  val commands : (string, Command.t) Hashtbl.t
  (** A table mapping commands to command names *)
end

module type Server = sig
  module type S = S

  module Make (C : Command.S) : S with module Store = C.Store
end
