open Irmin_server_types

module type S = sig
  type t

  type hash

  type contents

  type branch

  type commit

  type path

  type tree

  type step

  type slice

  type metadata

  type stats = Stats.t

  val stats_t : stats Irmin.Type.t

  val slice_t : slice Irmin.Type.t

  module Info : sig
    include Irmin.Info.S

    val init : ?author:string -> ?message:string -> int64 -> t

    val v : ?author:string -> ('b, Format.formatter, unit, f) format4 -> 'b
  end

  module Path : Irmin.Path.S with type t = path and type step = step

  module Hash : Irmin.Hash.S with type t = hash

  module Metadata : Irmin.Metadata.S with type t = metadata

  module Schema :
    Irmin.Schema.S
      with type Path.t = path
       and type Path.step = step
       and type Hash.t = hash
       and type Branch.t = branch
       and type Metadata.t = metadata

  module Private : sig
    module Store : Irmin.Generic_key.S with module Schema = Schema

    module Tree : Tree.S with module Private.Store = Store
  end

  type batch =
    (path
    * [ `Contents of [ `Hash of hash | `Value of contents ] * metadata option
      | `Tree of Private.Tree.t ]
      option)
    list

  val connect : ?batch_size:int -> ?tls:bool -> uri:Uri.t -> unit -> t Lwt.t
  (** Connect to the server specified by [uri] *)

  val uri : t -> Uri.t
  (** Get the URI the client is connected to *)

  val close : t -> unit Lwt.t
  (** Close connection to the server *)

  val dup : t -> t Lwt.t
  (** Duplicate a client. This will create a new connection with the same configuration *)

  val stats : t -> stats Error.result Lwt.t
  (** Get stats from the server *)

  val ping : t -> unit Error.result Lwt.t
  (** Ping the server *)

  val export : t -> slice Error.result Lwt.t

  val import : t -> slice -> unit Error.result Lwt.t

  val watch :
    (commit Irmin.Diff.t -> [ `Continue | `Stop ] Error.result Lwt.t) ->
    t ->
    unit Error.result Lwt.t
  (** Start watching for updates, calling the provided callback for each update.
      To continue watching return [Ok `Continue] and to stop return [Ok `Stop] *)

  module Commit : sig
    type key

    val key_t : key Irmin.Type.t

    val v :
      t -> info:Info.f -> parents:key list -> tree -> commit Error.result Lwt.t
    (** Create a new commit
        NOTE: this will invalidate all intermediate trees *)

    val of_key : t -> key -> commit option Error.result Lwt.t

    val key : commit -> key
    (** Get commit hash *)

    val parents : commit -> key list
    (** The commit parents. *)

    val info : commit -> Info.t
    (** The commit info. *)

    val t : commit Irmin.Type.t
    (** [t] is the value type for {!t}. *)

    val hash_t : hash Irmin.Type.t
    (** [hash_t] is the value type for {!hash}. *)

    val tree : t -> commit -> tree
    (** Commit tree *)

    type t = commit
  end

  module Contents : sig
    val of_hash : t -> hash -> contents option Error.result Lwt.t
    (** Find the contents associated with a hash *)

    val exists : t -> contents -> bool Error.result Lwt.t
    (** Check if [contents] exists in the store already *)

    val save : t -> contents -> hash Error.result Lwt.t
    (** Save value to store without associating it with a path *)

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
    type key

    val key_t : key Irmin.Type.t

    val split : tree -> t * Private.Tree.t * batch
    (** Get private fields from [Tree.t] *)

    val v : t -> ?batch:batch -> Private.Tree.t -> tree
    (** Create a new tree *)

    val of_key : t -> key -> tree
    (** Create a tree from a key that specifies a tree that already exists in the store *)

    val empty : t -> tree
    (** Create a new, empty tree *)

    val clear : tree -> unit Error.result Lwt.t
    (** Clear caches on the server for a given tree *)

    val key : tree -> key Error.result Lwt.t
    (** Get key of tree *)

    val build : t -> ?tree:Private.Tree.t -> batch -> tree Error.result Lwt.t
    (** [build store ~tree batch] performs a batch update of [tree], or
        an empty tree if not specified *)

    val add :
      tree -> path -> ?metadata:metadata -> contents -> tree Error.result Lwt.t
    (** Add contents to a tree, this may be batched so the update on the server
        could be delayed *)

    val add' : tree -> path -> contents -> tree Error.result Lwt.t
    (** Non-batch version of [add] *)

    val add_tree : tree -> path -> tree -> tree Error.result Lwt.t

    val add_tree' : tree -> path -> tree -> tree Error.result Lwt.t
    (** Non-batch version of [add_tree] *)

    val batch_update : tree -> batch -> tree Error.result Lwt.t
    (** Batch update tree *)

    val find : tree -> path -> contents option Error.result Lwt.t
    (** Find the value associated with the given path *)

    val find_tree : tree -> path -> tree option Error.result Lwt.t
    (** Find the tree associated with the given path *)

    val remove : tree -> path -> tree Error.result Lwt.t
    (** Remove value from a tree, returning a new tree *)

    val cleanup : tree -> unit Error.result Lwt.t
    (** Invalidate a tree, this frees the tree on the server side *)

    val cleanup_all : t -> unit Error.result Lwt.t
    (** Cleanup all trees *)

    val mem : tree -> path -> bool Error.result Lwt.t
    (** Check if a path is associated with a value *)

    val mem_tree : tree -> path -> bool Error.result Lwt.t
    (** Check if a path is associated with a tree *)

    val list :
      tree ->
      path ->
      (Path.step * [ `Contents | `Tree ]) list Error.result Lwt.t
    (** List entries at the specified root *)

    val merge : old:tree -> tree -> tree -> tree Error.result Lwt.t
    (** Three way merge *)

    module Local = Private.Tree.Local
    (*with type path = path
      and type contents = contents
      and type hash = hash
      and type step = Path.step
      and type t = Private.Store.tree*)

    val to_local : tree -> Local.t Error.result Lwt.t
    (** Exchange [tree], which may be a hash or ID, for a tree
        NOTE: this will encode the full tree and should be avoided if possible  *)

    val of_local : t -> Local.t -> tree Lwt.t
    (** Convert a local tree into a remote tree *)

    type t = tree
  end

  module Store : sig
    val find : t -> path -> contents option Error.result Lwt.t
    (** Find the value associated with a path, if it exists *)

    val find_tree : t -> path -> Tree.t option Error.result Lwt.t
    (** Find the tree associated with a path, if it exists *)

    val set : t -> info:Info.f -> path -> contents -> unit Error.result Lwt.t
    (** Associate a new value with the given path *)

    val test_and_set :
      t ->
      info:Info.f ->
      path ->
      test:contents option ->
      set:contents option ->
      unit Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value *)

    val remove : t -> info:Info.f -> path -> unit Error.result Lwt.t
    (** Remove a value from the store *)

    val set_tree :
      t -> info:Info.f -> path -> Tree.t -> Tree.t Error.result Lwt.t
    (** Set a tree at the given path *)

    val test_and_set_tree :
      t ->
      info:Info.f ->
      path ->
      test:Tree.t option ->
      set:Tree.t option ->
      Tree.t option Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value *)

    val mem : t -> path -> bool Error.result Lwt.t
    (** Check if the given path has an associated value *)

    val mem_tree : t -> path -> bool Error.result Lwt.t
    (** Check if the given path has an associated tree *)

    val merge : t -> info:Info.f -> branch -> unit Error.result Lwt.t
    (** Merge the current branch with the provided branch *)

    val merge_commit : t -> info:Info.f -> Commit.t -> unit Error.result Lwt.t
    (** Merge the current branch with the provided commit *)

    val last_modified : t -> path -> Commit.t list Error.result Lwt.t
    (** Get a list of commits that modified the specified path *)
  end
end

module type Client = sig
  module type S = S

  module Make (C : Command.S) :
    S
      with type hash = C.Store.hash
       and type contents = C.Store.contents
       and type branch = C.Store.branch
       and type path = C.Store.path
       and type commit = C.Commit.t
       and type step = C.Store.step
       and type metadata = C.Store.metadata
       and type slice = C.Store.slice
       and module Schema = C.Store.Schema
       and type Private.Store.tree = C.Store.tree
end
