module type S = sig
  type conf = Conduit_lwt_unix.client

  type t

  type hash

  type contents

  type branch

  type commit

  type key

  type tree

  module Key : Irmin.Path.S with type t = key

  module Hash : Irmin.Hash.S with type t = hash

  module Contents : Irmin.Contents.S with type t = contents

  module Branch : Irmin.Branch.S with type t = branch

  module Private : sig
    module Tree :
      Tree.S
        with type Private.Store.hash = hash
         and type Private.Store.contents = contents
         and type Private.Store.branch = branch
         and type Private.Store.key = key
  end

  val connect : ?tls:bool -> uri:string -> unit -> t Lwt.t

  val ping : t -> unit Error.result Lwt.t

  val set_branch : t -> branch -> unit Error.result Lwt.t

  module Tree : sig
    val empty : t -> tree Error.result Lwt.t

    val add : tree -> key -> contents -> tree Error.result Lwt.t

    val remove : tree -> key -> tree Error.result Lwt.t

    val abort : tree -> unit Error.result Lwt.t

    val mem : tree -> key -> bool Error.result Lwt.t

    val mem_tree : tree -> key -> bool Error.result Lwt.t

    val list :
      tree -> key -> (Key.step * [ `Contents | `Tree ]) list Error.result Lwt.t

    module Local :
      Tree_intf.LOCAL
        with type key = key
         and type contents = contents
         and type hash = hash
         and type step = Key.step

    val of_local : t -> Local.t -> tree

    type t = tree
  end

  module Store : sig
    val find : t -> key -> contents option Error.result Lwt.t

    val find_tree : t -> key -> Tree.t option Error.result Lwt.t

    val set :
      t -> info:Irmin.Info.f -> key -> contents -> unit Error.result Lwt.t

    val test_and_set :
      t ->
      info:Irmin.Info.f ->
      key ->
      test:contents option ->
      set:contents option ->
      unit Error.result Lwt.t

    val remove : t -> info:Irmin.Info.f -> key -> unit Error.result Lwt.t

    val set_tree :
      t -> info:Irmin.Info.f -> key -> Tree.t -> unit Error.result Lwt.t

    val test_and_set_tree :
      t ->
      info:Irmin.Info.f ->
      key ->
      test:Tree.t option ->
      set:Tree.t option ->
      unit Error.result Lwt.t

    val mem : t -> key -> bool Error.result Lwt.t

    val mem_tree : t -> key -> bool Error.result Lwt.t
  end

  module Commit : sig
    include Irmin.Private.Commit.S with type hash = hash
  end
end

module type Client = sig
  module type S = S

  module Make (C : Command.S) :
    S
      with type hash = C.Store.hash
       and type contents = C.Store.contents
       and type branch = C.Store.branch
       and type key = string list
end
