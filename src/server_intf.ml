module type S = sig
  type t

  module Store : Irmin_pack_layered.S

  module Command : Command.S with module Store = Store

  val v : ?ctx:Conduit_lwt_unix.ctx -> port:int -> Irmin.config -> t Lwt.t

  val serve : ?http:bool -> t -> unit Lwt.t

  val commands : (string, Command.t) Hashtbl.t
end

module type Server = sig
  module type S = S

  module Make (C : Command.S) : S with module Store = C.Store
end
