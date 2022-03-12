module type S = sig
  module Conn : Conn.S

  module Store : Irmin.Generic_key.S
  (** Irmin [Store] type *)

  (** [Tree] wraps [Store.Tree] to avoid encoding/decoding trees more than needed *)
  module Tree :
    Tree.S with module Private.Store = Store and type Local.t = Store.tree

  (** Commits *)
  module Commit :
    Commit.S
      with type hash = Store.Hash.t
       and type key = Store.commit_key
       and type tree = Tree.t
       and module Info = Store.Info

  (** Used to track information about the server *)
  module Server_info : sig
    type t = { start_time : float }
  end

  type context = {
    conn : Conn.t;
    config : Irmin.Backend.Conf.t;
    repo : Store.Repo.t;
    mutable branch : Store.branch;
    mutable store : Store.t;
    trees : (int, Store.tree) Hashtbl.t;
    mutable watch : Store.watch option;
    mutable branch_watch : Store.Backend.Branch.watch option;
  }
  (** [context] is passed to every command as the first argument *)

  module Stats : sig
    type t = Stats.t

    val t : t Irmin.Type.t
    val v : context -> Server_info.t -> t Lwt.t
    val to_json : t -> string
  end

  module type CMD = sig
    type req
    type res

    val req_t : req Irmin.Type.t
    val res_t : res Irmin.Type.t
    val name : string

    val run :
      Conn.t -> context -> Server_info.t -> req -> res Conn.Return.t Lwt.t
  end

  type t = (module CMD)
  (** Command type *)

  val name : t -> string
  (** Get the name of a command *)

  val of_name : string -> t
  (** Find a command by name, the name should be lowercase *)

  val commands : (string * t) list
  (** A list of all registered commands *)

  module Commands : sig
    module Backend : sig
      module Schema : Irmin.Schema.S
      module Hash : Irmin.Hash.S with type t = Schema.Hash.t

      module Contents : sig
        type key = Store.Backend.Contents.key
        type value = Store.Backend.Contents.value
        type hash = Store.Backend.Contents.hash

        module Mem : CMD with type req = key and type res = bool
        module Find : CMD with type req = key and type res = value option
        module Add : CMD with type req = value and type res = key
        module Unsafe_add : CMD with type req = hash * value and type res = key
        module Index : CMD with type req = hash and type res = key option
        module Clear : CMD with type req = unit and type res = unit

        module Merge :
          CMD
            with type req = key option option * key option * key option
             and type res = (key option, Irmin.Merge.conflict) Result.t
      end

      module Node : sig
        type key = Store.Backend.Node.key
        type value = Store.Backend.Node.value
        type hash = Store.Backend.Node.hash

        module Mem : CMD with type req = key and type res = bool
        module Find : CMD with type req = key and type res = value option
        module Add : CMD with type req = value and type res = key
        module Unsafe_add : CMD with type req = hash * value and type res = key
        module Index : CMD with type req = hash and type res = key option
        module Clear : CMD with type req = unit and type res = unit

        module Merge :
          CMD
            with type req = key option option * key option * key option
             and type res = (key option, Irmin.Merge.conflict) Result.t
      end

      module Commit : sig
        type key = Store.Backend.Commit.key
        type value = Store.Backend.Commit.value
        type hash = Store.Backend.Commit.hash

        module Mem : CMD with type req = key and type res = bool
        module Find : CMD with type req = key and type res = value option
        module Add : CMD with type req = value and type res = key
        module Unsafe_add : CMD with type req = hash * value and type res = key
        module Index : CMD with type req = hash and type res = key option
        module Clear : CMD with type req = unit and type res = unit

        module Merge :
          CMD
            with type req =
              Store.Info.t * (key option option * key option * key option)
             and type res = (key option, Irmin.Merge.conflict) Result.t
      end

      module Branch : sig
        type key = Store.Backend.Branch.key
        type value = Store.Backend.Branch.value

        module Mem : CMD with type req = key and type res = bool
        module Find : CMD with type req = key and type res = value option
        module Set : CMD with type req = key * value and type res = unit

        module Test_and_set :
          CMD
            with type req = key * value option * value option
             and type res = bool

        module Remove : CMD with type req = key and type res = unit
        module List : CMD with type req = unit and type res = key list
        module Clear : CMD with type req = unit and type res = unit

        module Watch :
          CMD with type req = (key * value) list option and type res = unit

        module Watch_key :
          CMD with type req = value option * key and type res = unit

        module Unwatch : CMD with type req = unit and type res = unit
      end
    end

    (** Get statistics from the server *)
    module Stats : CMD with type req = unit and type res = Stats.t

    (** Check connectivity *)
    module Ping : CMD with type req = unit and type res = unit

    (* Branch *)

    (** Set the current branch for a client *)
    module Set_current_branch :
      CMD with type req = Store.branch and type res = unit

    (** Get the current branch for a client *)
    module Get_current_branch :
      CMD with type req = unit and type res = Store.branch

    (** Export repo *)
    module Export : CMD with type req = int option and type res = Store.slice

    (** Import repo *)
    module Import : CMD with type req = Store.slice and type res = unit

    (* Branch *)

    (** Get latest commit for a branch *)
    module Branch_head :
      CMD with type req = Store.branch option and type res = Commit.t option

    (** Set the latest commit for a branch *)
    module Branch_set_head :
      CMD with type req = Store.branch option * Commit.t and type res = unit

    (** Remove a branch *)
    module Branch_remove : CMD with type req = Store.branch and type res = unit

    (* Commit *)

    (** Create a new commit *)
    module Commit_v :
      CMD
        with type req = Store.Info.t * Store.commit_key list * Tree.t
         and type res = Commit.t

    (** Find a commit by key *)
    module Commit_of_key :
      CMD with type req = Store.commit_key and type res = Commit.t option

    (** Convert from commit key to commit hash *)
    module Commit_hash_of_key :
      CMD with type req = Store.commit_key and type res = Store.Hash.t option

    (** Find a commit by hash *)
    module Commit_of_hash :
      CMD with type req = Store.hash and type res = Commit.t option

    (* Contents *)

    (** Find contents by key *)
    module Contents_of_hash :
      CMD with type req = Store.hash and type res = Store.contents option

    (** Check if contents that match the provided hash can be found in the store *)
    module Contents_exists : CMD with type req = Store.hash and type res = bool

    (** Add contents to repo *)
    module Contents_save :
      CMD with type req = Store.contents and type res = Store.contents_key

    (** Watch for changes *)
    module Watch : CMD with type req = unit and type res = unit

    (** Remove watcher from client *)
    module Unwatch : CMD with type req = unit and type res = unit

    (* Store *)
    module Store : sig
      (** Find a value in the store *)
      module Find :
        CMD with type req = Store.path and type res = Store.contents option

      (** Add a new value to the store *)
      module Set :
        CMD
          with type req = Store.path * Store.Info.t * Store.contents
           and type res = unit

      (** Add a value to the store if [test] matches the existing value *)
      module Test_and_set :
        CMD
          with type req =
            Store.path
            * Store.Info.t
            * (Store.contents option * Store.contents option)
           and type res = unit

      (** Remove a value from the store *)
      module Remove :
        CMD with type req = Store.path * Store.Info.t and type res = unit

      (** Get a tree from the store *)
      module Find_tree :
        CMD with type req = Store.path and type res = Tree.t option

      (** Add a tree to the store *)
      module Set_tree :
        CMD
          with type req = Store.path * Store.Info.t * Tree.t
           and type res = Tree.t

      (** Add a tree to the store if [test] matches the existing tree *)
      module Test_and_set_tree :
        CMD
          with type req =
            Store.path * Store.Info.t * (Tree.t option * Tree.t option)
           and type res = Tree.t option

      (** Check for the existence of a value in the store *)
      module Mem : CMD with type req = Store.path and type res = bool

      (** Check for the existence of a tree in the store *)
      module Mem_tree : CMD with type req = Store.path and type res = bool

      (** Merge the current branch with another branch *)
      module Merge :
        CMD with type req = Store.Info.t * Store.Branch.t and type res = unit

      (** Merge the current branch with a commit *)
      module Merge_commit :
        CMD with type req = Store.Info.t * Commit.t and type res = unit

      (** Get a list of commits that modified a specific path *)
      module Last_modified :
        CMD with type req = Store.path and type res = Commit.t list
    end

    (* Tree *)
    module Tree : sig
      (** Create an empty tree *)
      module Empty : CMD with type req = unit and type res = Tree.t

      (** Add a value to a tree *)
      module Add :
        CMD
          with type req =
            Tree.t * Tree.Private.Store.path * Tree.Private.Store.contents
           and type res = Tree.t

      module Save :
        CMD
          with type req = Tree.t
           and type res =
            [ `Contents of Tree.Private.Store.contents_key
            | `Node of Tree.Private.Store.node_key ]

      (** Add multiple trees/values to a tree *)
      module Batch_update :
        CMD
          with type req =
            Tree.t
            * (Tree.Private.Store.path
              * [ `Contents of
                  [ `Hash of Tree.Private.Store.hash
                  | `Value of Tree.Private.Store.contents ]
                  * Tree.Private.Store.metadata option
                | `Tree of Tree.t ]
                option)
              list
           and type res = Tree.t

      (** Add a tree to a tree *)
      module Add_tree :
        CMD
          with type req = Tree.t * Tree.Private.Store.path * Tree.t
           and type res = Tree.t

      (** Remove path from a tree *)
      module Remove :
        CMD
          with type req = Tree.t * Tree.Private.Store.path
           and type res = Tree.t

      (** Find a value from a tree *)
      module Find :
        CMD
          with type req = Tree.t * Tree.Private.Store.path
           and type res = Tree.Private.Store.contents option

      (** Find a tree from a tree *)
      module Find_tree :
        CMD
          with type req = Tree.t * Tree.Private.Store.path
           and type res = Tree.t option

      (** Deallocate a single tree *)
      module Cleanup : CMD with type req = Tree.t and type res = unit

      (** Convert tree to concrete representation *)
      module To_local :
        CMD with type req = Tree.t and type res = Tree.Local.concrete

      (** Check if a path is set to a value in a tree *)
      module Mem :
        CMD with type req = Tree.t * Tree.Private.Store.path and type res = bool

      (** Check if a path is set to a tree *)
      module Mem_tree :
        CMD with type req = Tree.t * Tree.Private.Store.path and type res = bool

      (** List items in one level of a tree *)
      module List :
        CMD
          with type req = Tree.t * Tree.Private.Store.path
           and type res =
            (Tree.Private.Store.Path.step * [ `Contents | `Tree ]) list

      (** Clear tree cache *)
      module Clear : CMD with type req = Tree.t and type res = unit

      (** Get tree hash *)
      module Hash :
        CMD with type req = Tree.t and type res = Tree.Private.Store.Hash.t

      (** Get tree key *)
      module Key :
        CMD
          with type req = Tree.t
           and type res = Tree.Private.Store.Tree.kinded_key

      (** Deallocate all trees *)
      module Cleanup_all : CMD with type req = unit and type res = unit

      (** Merge with another tree *)
      module Merge :
        CMD with type req = Tree.t * Tree.t * Tree.t and type res = Tree.t
    end
  end
end

module type Command = sig
  module type S = S

  module Make
      (IO : Conn.IO)
      (Codec : Conn.Codec.S)
      (Store : Irmin.Generic_key.S) :
    S
      with module Store = Store
       and module Tree.Private.Store = Store
       and type Tree.Local.t = Store.tree
       and module Conn.IO = IO
end
