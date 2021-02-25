module type S = sig
  type branch = string

  type hash

  type contents

  module Server : sig
    type t

    val v : ?ctx:Conduit_lwt_unix.ctx -> port:int -> Irmin.config -> t Lwt.t

    val serve : t -> unit Lwt.t
  end

  module Client : sig
    type conf

    type t

    val conf : ?host:string -> port:int -> unit -> conf

    val connect : ?ctx:Conduit_lwt_unix.ctx -> conf -> t Lwt.t

    val ping : t -> (unit, Error.t) result Lwt.t

    val set_branch : t -> branch -> (unit, Error.t) result Lwt.t
  end
end

module Command = Command

module type Irmin_server = sig
  module Command = Command
  module Args = Args
  module Error = Error

  module Make (H : Irmin.Hash.S) (C : Irmin.Contents.S) :
    S with type hash = H.t and type contents = C.t
end
