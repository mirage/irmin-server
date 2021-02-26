module type S = sig
  type t

  module Store : Irmin.S

  val v : ?ctx:Conduit_lwt_unix.ctx -> port:int -> Irmin.config -> t Lwt.t

  val serve : t -> unit Lwt.t
end

module type Server = sig
  module type S = S

  module Make (C : Command.S) : S with module Store = C.Store
end
