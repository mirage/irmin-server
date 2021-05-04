open Irmin_server_types

module type S = sig
  type t

  type hash

  type contents

  type branch

  type commit

  type key

  type tree

  type step

  type slice

  type metadata

  type stats = Stats.t

  val stats_t : stats Irmin.Type.t

  val slice_t : slice Irmin.Type.t

  module Key : Irmin.Path.S with type t = key and type step = step

  module Hash : Irmin.Hash.S with type t = hash

  module Metadata : Irmin.Metadata.S with type t = metadata

  module Private : sig
    module Store :
      Irmin.S
        with type hash = hash
         and type contents = contents
         and type branch = branch
         and type key = key
         and type step = step
         and type slice = slice
         and type metadata = metadata

    module Tree :
      Tree.S
        with type Private.Store.hash = hash
         and type Private.Store.contents = contents
         and type Private.Store.branch = branch
  end

  type batch =
    (key
    * [ `Contents of [ `Hash of hash | `Value of contents ]
      | `Tree of Private.Tree.t ])
    list

  val connect : ?batch_size:int -> ?tls:bool -> uri:Uri.t -> unit -> t Lwt.t
  (** Connect to the server specified by [uri] *)

  val uri : t -> Uri.t

  val close : t -> unit Lwt.t

  val dup : t -> t Lwt.t

  val stats : t -> stats Error.result Lwt.t

  val ping : t -> unit Error.result Lwt.t
  (** Ping the server *)

  val export : t -> slice Error.result Lwt.t

  val import : t -> slice -> unit Error.result Lwt.t

  module Commit : sig
    val v :
      t ->
      info:Irmin.Info.f ->
      parents:hash list ->
      tree ->
      commit Error.result Lwt.t
    (** Create a new commit
        NOTE: this will invalidate all intermediate trees *)

    val of_hash : t -> hash -> commit option Error.result Lwt.t

    val hash : commit -> hash
    (** Get commit hash *)

    val parents : commit -> hash list
    (** The commit parents. *)

    val info : commit -> Irmin.Info.t
    (** The commit info. *)

    val t : commit Irmin.Type.t
    (** [t] is the value type for {!t}. *)

    val hash_t : hash Irmin.Type.t
    (** [hash_t] is the value type for {!hash}. *)

    val tree : t -> commit -> tree

    type t = commit
  end

  module Contents : sig
    val of_hash : t -> hash -> contents option Error.result Lwt.t

    val exists : t -> contents -> bool Error.result Lwt.t

    val save : t -> contents -> hash Error.result Lwt.t

    include Irmin.Contents.S with type t = contents
  end

  module Branch : sig
    val set_current : t -> branch -> unit Error.result Lwt.t
    (** Set the current branch for a single connection *)

    val get_current : t -> branch Error.result Lwt.t
    (** Get the branch for a connection *)

    val get : ?branch:branch -> t -> commit option Error.result Lwt.t
    (** Get the head commit for the given branch, or the current branch if none is specified *)

    val set : ?branch:branch -> t -> commit -> unit Error.result Lwt.t
    (** Set the head commit for the given branch, or the current branch if none is specified *)

    val remove : t -> branch -> unit Error.result Lwt.t
    (** Delete a branch *)

    include Irmin.Branch.S with type t = branch
  end

  module Tree : sig
    val split : tree -> t * Private.Tree.t * batch

    val v : t -> ?batch:batch -> Private.Tree.t -> tree

    val of_hash : t -> hash -> tree

    val empty : t -> tree
    (** Create a new, empty tree *)

    val clear : tree -> unit Error.result Lwt.t
    (** Clear caches on the server for a given tree *)

    val hash : tree -> hash Error.result Lwt.t
    (** Get hash of tree *)

    val build : t -> ?tree:Private.Tree.t -> batch -> tree Error.result Lwt.t
    (** [build store ~tree batch] performs a batch update of [tree], or
        an empty tree if not specified *)

    val add : tree -> key -> contents -> tree Error.result Lwt.t
    (** Add contents to a tree, this may be batched so the update on the server
        could be delayed *)

    val add' : tree -> key -> contents -> tree Error.result Lwt.t
    (** Non-batch version of [add] *)

    val add_tree : tree -> key -> tree -> tree Error.result Lwt.t

    val add_tree' : tree -> key -> tree -> tree Error.result Lwt.t
    (** Non-batch version of [add_tree] *)

    val add_batch : tree -> batch -> tree Error.result Lwt.t
    (** Batch update tree *)

    val find : tree -> key -> contents option Error.result Lwt.t
    (** Find the value associated with the given key *)

    val find_tree : tree -> key -> tree option Error.result Lwt.t
    (** Find the tree associated with the given key *)

    val remove : tree -> key -> tree Error.result Lwt.t
    (** Remove value from a tree, returning a new tree *)

    val cleanup : tree -> unit Error.result Lwt.t
    (** Invalidate a tree, this frees the tree on the server side *)

    val cleanup_all : t -> unit Error.result Lwt.t
    (** Cleanup all trees *)

    val mem : tree -> key -> bool Error.result Lwt.t
    (** Check if a key is associated with a value *)

    val mem_tree : tree -> key -> bool Error.result Lwt.t
    (** Check if a key is associated with a tree *)

    val list :
      tree -> key -> (Key.step * [ `Contents | `Tree ]) list Error.result Lwt.t
    (** List entries at the specified root *)

    val merge : old:tree -> tree -> tree -> tree Error.result Lwt.t

    module Local :
      Tree.LOCAL
        with type key = key
         and type contents = contents
         and type hash = hash
         and type step = Key.step

    val to_local : tree -> Local.t Error.result Lwt.t
    (** Exchange [tree], which may be a hash or ID, for a tree
        NOTE: this will encode the full tree  *)

    val of_local : t -> Local.t -> tree Lwt.t
    (** Convert a local tree into a remote tree *)

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
        NOTE: the tree parameter will not be valid after this call, the
        returned tree should be used instead *)

    val test_and_set_tree :
      t ->
      info:Irmin.Info.f ->
      key ->
      test:Tree.t option ->
      set:Tree.t option ->
      Tree.t option Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value
        NOTE: the tree parameter will not be valid after this call, the
        returned tree should be used instead *)

    val mem : t -> key -> bool Error.result Lwt.t
    (** Check if the given key has an associated value *)

    val mem_tree : t -> key -> bool Error.result Lwt.t
    (** Check if the given key has an associated tree *)

    val merge : t -> info:Irmin.Info.f -> branch -> unit Error.result Lwt.t

    val merge_commit :
      t -> info:Irmin.Info.f -> Commit.t -> unit Error.result Lwt.t

    val last_modified : t -> key -> Commit.t list Error.result Lwt.t

    val watch :
      (Commit.t Irmin.Diff.t -> [ `Continue | `Stop ] Error.result Lwt.t) ->
      t ->
      unit Error.result Lwt.t
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
       and type step = C.Store.step
       and type metadata = C.Store.metadata
end
