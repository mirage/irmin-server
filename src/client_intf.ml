module type S = sig
  type conf = Conduit_lwt_unix.client

  type t

  type hash

  type contents

  type branch

  type commit

  type key

  type tree

  type slice

  val slice_t : slice Irmin.Type.t

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
  end

  module Commit : sig
    include Irmin.Private.Commit.S with type hash = hash and type t = commit
  end

  val connect : ?tls:bool -> uri:string -> unit -> t Lwt.t
  (** Connect to the server specified by [uri] *)

  val ping : t -> unit Error.result Lwt.t
  (** Ping the server *)

  val set_branch : t -> branch -> unit Error.result Lwt.t
  (** Set the current branch for a single connection *)

  val get_branch : t -> branch Error.result Lwt.t
  (** Get the branch for a connection *)

  val head : ?branch:branch -> t -> commit option Error.result Lwt.t

  val export : t -> slice Error.result Lwt.t

  val import : t -> slice -> unit Error.result Lwt.t

  module Tree : sig
    val empty : t -> tree Error.result Lwt.t
    (** Create a new, empty tree *)

    val add : tree -> key -> contents -> tree Error.result Lwt.t
    (** Add values to a tree, returning a new tree
        NOTE: the tree that was passed in may no longer be valid
        after this call *)

    val remove : tree -> key -> tree Error.result Lwt.t
    (** Remove value from a tree, returning a new tree
        NOTE: the tree that was passed in may no longer be valid
        after this call *)

    val clone : tree -> tree Error.result Lwt.t
    (** Copies an existing tree, this can be used to create a new copy of a tree before passing it to a
        function that may invalidate it *)

    val abort : tree -> unit Error.result Lwt.t
    (** Invalidate a tree, this frees the tree on the server side *)

    val mem : tree -> key -> bool Error.result Lwt.t
    (** Check if a key is associated with a value *)

    val mem_tree : tree -> key -> bool Error.result Lwt.t
    (** Check if a key is associated with a tree *)

    val list :
      tree -> key -> (Key.step * [ `Contents | `Tree ]) list Error.result Lwt.t
    (** List entries at the specified root *)

    module Local :
      Tree_intf.LOCAL
        with type key = key
         and type contents = contents
         and type hash = hash
         and type step = Key.step

    val to_local : tree -> Local.t Error.result Lwt.t
    (** Exchange [tree], which may be a hash or ID, for a tree
        NOTE: this will encode the full tree  *)

    val of_local : t -> Local.t -> tree

    type t = tree
  end

  module Store : sig
    val find : t -> key -> contents option Error.result Lwt.t
    (** Find the value associated with a key, if it exists *)

    val find_tree : t -> key -> Tree.t option Error.result Lwt.t
    (** Find the tree associated with a key, if it exists *)

    val set :
      t -> info:Irmin.Info.f -> key -> contents -> unit Error.result Lwt.t
    (** Associate a new value with the given key *)

    val test_and_set :
      t ->
      info:Irmin.Info.f ->
      key ->
      test:contents option ->
      set:contents option ->
      unit Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value *)

    val remove : t -> info:Irmin.Info.f -> key -> unit Error.result Lwt.t
    (** Remove a value from the store *)

    val set_tree :
      t -> info:Irmin.Info.f -> key -> Tree.t -> Tree.t Error.result Lwt.t
    (** Set a tree at the given key
        NOTE: the tree parameter may no longer be valid after this call, the
        returned tree should be used instead *)

    val test_and_set_tree :
      t ->
      info:Irmin.Info.f ->
      key ->
      test:Tree.t option ->
      set:Tree.t option ->
      Tree.t option Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value
        NOTE: the tree parameter may no longer be valid after this call, the
        returned tree should be used instead *)

    val mem : t -> key -> bool Error.result Lwt.t
    (** Check if the given key has an associated value *)

    val mem_tree : t -> key -> bool Error.result Lwt.t
    (** Check if the given key has an associated tree *)
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
       and type commit = C.Commit.t
end
