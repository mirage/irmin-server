module type S = sig
  type conf

  type t

  type hash

  type contents

  type branch

  type key

  val conf : ?host:string -> port:int -> unit -> conf

  val connect : ?ctx:Conduit_lwt_unix.ctx -> conf -> t Lwt.t

  val ping : t -> unit Error.result Lwt.t

  val set_branch : t -> branch -> unit Error.result Lwt.t

  module Tree : sig
    type store = t

    include
      Tree.S
        with type Private.Store.hash = hash
         and type Private.Store.contents = contents
         and type Private.Store.branch = branch
         and type Private.Store.key = key

    (*val empty : store -> t Error.result Lwt.t

      val add : store -> t -> key -> contents -> t Error.result Lwt.t

      val remove : store -> t -> key -> t Error.result Lwt.t*)
  end

  module Store : sig
    val find : t -> key -> contents option Error.result Lwt.t

    (*val find_tree : t -> key -> Tree.t option Error.result Lwt.t

      val set :
        t -> info:Irmin.Info.f -> key -> contents -> unit Error.result Lwt.t

      val remove : t -> info:Irmin.Info.f -> key -> unit Error.result Lwt.t

      val set_tree :
        t -> info:Irmin.Info.f -> key -> Tree.t -> Tree.t Error.result Lwt.t*)
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
