module type S = sig
  type conf

  type t

  type hash

  type contents

  type branch

  type key

  val conf : ?host:string -> port:int -> unit -> conf

  val connect : ?ctx:Conduit_lwt_unix.ctx -> conf -> t Lwt.t

  val ping : t -> (unit, Error.t) result Lwt.t

  val set_branch : t -> branch -> (unit, Error.t) result Lwt.t

  module Store : sig
    val find : t -> key -> (contents option, Error.t) result Lwt.t

    val set :
      t -> info:Irmin.Info.f -> key -> contents -> (unit, Error.t) result Lwt.t

    val remove : t -> info:Irmin.Info.f -> key -> (unit, Error.t) result Lwt.t
  end
end

module type Client = sig
  module type S = S

  module Make (C : Command.S) :
    S
      with type hash = C.Store.hash
       and type contents = C.Store.contents
       and type branch = C.Store.branch
       and type key = C.Store.key
end
