open Irmin_server_types

module type S = sig
  type t

  module Store : Irmin.Generic_key.S

  module Command : Command.S with module Store = Store

  val readonly : Irmin.config -> Irmin.config

  val v :
    ?tls_config:[ `Cert_file of string ] * [ `Key_file of string ] ->
    uri:Uri.t ->
    Irmin.config ->
    t Lwt.t
  (** Create an instance of the server *)

  val serve : ?stop:unit Lwt.t -> t -> unit Lwt.t
  (** Run the server *)

  val commands : (string, Command.t) Hashtbl.t
  (** A table mapping commands to command names *)
end

module type Server = sig
  module type S = S

  module Make (C : Command.S) : S with module Store = C.Store
end
