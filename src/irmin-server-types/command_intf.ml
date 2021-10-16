module type S = sig
  module Store : Irmin.S
  (** Irmin [Store] type *)

  (** [Tree] wraps [Store.Tree] to avoid encoding/decoding trees more than needed *)
  module Tree :
    Tree.S with module Private.Store = Store and type Local.t = Store.tree

  (** Commits *)
  module Commit :
    Commit.S
      with type hash = Store.Hash.t
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
  }
  (** [context] is passed to every command as the first argument *)

  module Stats : sig
    type t = Stats.t

    val t : t Irmin.Type.t

    val v : context -> Server_info.t -> t Lwt.t

    val to_json : t -> string
  end

  module type CMD = sig
    module Req : sig
      type t

      val t : t Irmin.Type.t
    end

    module Res : sig
      type t

      val t : t Irmin.Type.t
    end

    val name : string

    val run :
      Conn.t -> context -> Server_info.t -> Req.t -> Res.t Return.t Lwt.t
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
    (** Get statistics from the server *)
    module Stats : CMD with type Req.t = unit and type Res.t = Stats.t

    (** Check connectivity *)
    module Ping : CMD with type Req.t = unit and type Res.t = unit

    (* Branch *)

    (** Set the current branch for a client *)
    module Set_current_branch :
      CMD with type Req.t = Store.branch and type Res.t = unit

    (** Get the current branch for a client *)
    module Get_current_branch :
      CMD with type Req.t = unit and type Res.t = Store.branch

    (** Export repo *)
    module Export : CMD with type Req.t = unit and type Res.t = Store.slice

    (** Import repo *)
    module Import : CMD with type Req.t = Store.slice and type Res.t = unit

    (* Branch *)

    (** Get latest commit for a branch *)
    module Branch_head :
      CMD with type Req.t = Store.branch option and type Res.t = Commit.t option

    (** Set the latest commit for a branch *)
    module Branch_set_head :
      CMD with type Req.t = Store.branch option * Commit.t and type Res.t = unit

    (** Remove a branch *)
    module Branch_remove :
      CMD with type Req.t = Store.branch and type Res.t = unit

    (* Commit *)

    (** Create a new commit *)
    module Commit_v :
      CMD
        with type Req.t = Store.Info.t * Store.hash list * Tree.t
         and type Res.t = Commit.t

    (** Find a commit by hash *)
    module Commit_of_hash :
      CMD with type Req.t = Store.Hash.t and type Res.t = Commit.t option

    (* Contents *)

    (** Find contents by hash *)
    module Contents_of_hash :
      CMD with type Req.t = Store.Hash.t and type Res.t = Store.contents option

    (** Check if contents that match the provided hash can be found in the store *)
    module Contents_exists :
      CMD with type Req.t = Store.Hash.t and type Res.t = bool

    (** Add contents to repo *)
    module Contents_save :
      CMD with type Req.t = Store.contents and type Res.t = Store.Hash.t

    (** Watch for changes *)
    module Watch : CMD with type Req.t = unit and type Res.t = unit

    (** Remove watcher from client *)
    module Unwatch : CMD with type Req.t = unit and type Res.t = unit

    (* Store *)
    module Store : sig
      (** Find a value in the store *)
      module Find :
        CMD with type Req.t = Store.key and type Res.t = Store.contents option

      (** Add a new value to the store *)
      module Set :
        CMD
          with type Req.t = Store.key * Store.Info.t * Store.contents
           and type Res.t = unit

      (** Add a value to the store if [test] matches the existing value *)
      module Test_and_set :
        CMD
          with type Req.t =
                Store.key
                * Store.Info.t
                * (Store.contents option * Store.contents option)
           and type Res.t = unit

      (** Remove a value from the store *)
      module Remove :
        CMD with type Req.t = Store.key * Store.Info.t and type Res.t = unit

      (** Get a tree from the store *)
      module Find_tree :
        CMD with type Req.t = Store.key and type Res.t = Tree.t option

      (** Add a tree to the store *)
      module Set_tree :
        CMD
          with type Req.t = Store.key * Store.Info.t * Tree.t
           and type Res.t = Tree.t

      (** Add a tree to the store if [test] matches the existing tree *)
      module Test_and_set_tree :
        CMD
          with type Req.t =
                Store.key * Store.Info.t * (Tree.t option * Tree.t option)
           and type Res.t = Tree.t option

      (** Check for the existence of a value in the store *)
      module Mem : CMD with type Req.t = Store.key and type Res.t = bool

      (** Check for the existence of a tree in the store *)
      module Mem_tree : CMD with type Req.t = Store.key and type Res.t = bool

      (** Merge the current branch with another branch *)
      module Merge :
        CMD
          with type Req.t = Store.Info.t * Store.Branch.t
           and type Res.t = unit

      (** Merge the current branch with a commit *)
      module Merge_commit :
        CMD with type Req.t = Store.Info.t * Commit.t and type Res.t = unit

      (** Get a list of commits that modified a specific key *)
      module Last_modified :
        CMD with type Req.t = Store.key and type Res.t = Commit.t list
    end

    (* Tree *)
    module Tree : sig
      (** Create an empty tree *)
      module Empty : CMD with type Req.t = unit and type Res.t = Tree.t

      (** Add a value to a tree *)
      module Add :
        CMD
          with type Req.t =
                Tree.t * Tree.Private.Store.key * Tree.Private.Store.contents
           and type Res.t = Tree.t

      (** Add multiple trees/values to a tree *)
      module Add_batch :
        CMD
          with type Req.t =
                Tree.t
                * (Tree.Private.Store.key
                  * [ `Contents of
                      [ `Hash of Tree.Private.Store.hash
                      | `Value of Tree.Private.Store.contents ]
                    | `Tree of Tree.t ])
                  list
           and type Res.t = Tree.t

      (** Add a tree to a tree *)
      module Add_tree :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key * Tree.t
           and type Res.t = Tree.t

      (** Remove key from a tree *)
      module Remove :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = Tree.t

      (** Find a value from a tree *)
      module Find :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = Tree.Private.Store.contents option

      (** Find a tree from a tree *)
      module Find_tree :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = Tree.t option

      (** Deallocate a single tree *)
      module Cleanup : CMD with type Req.t = Tree.t and type Res.t = unit

      (** Convert tree to concrete representation *)
      module To_local :
        CMD with type Req.t = Tree.t and type Res.t = Tree.Local.concrete

      (** Check if a key is set to a value in a tree *)
      module Mem :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = bool

      (** Check if a key is set to a tree *)
      module Mem_tree :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = bool

      (** List items in one level of a tree *)
      module List :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t =
                (Tree.Private.Store.Key.step * [ `Contents | `Tree ]) list

      (** Clear tree cache *)
      module Clear : CMD with type Req.t = Tree.t and type Res.t = unit

      (** Get tree hash *)
      module Hash :
        CMD with type Req.t = Tree.t and type Res.t = Tree.Private.Store.Hash.t

      (** Deallocate all trees *)
      module Cleanup_all : CMD with type Req.t = unit and type Res.t = unit

      (** Merge with another tree *)
      module Merge :
        CMD with type Req.t = Tree.t * Tree.t * Tree.t and type Res.t = Tree.t
    end
  end
end

module type Command = sig
  module type S = S

  module Make (Store : Irmin.S) :
    S
      with module Store = Store
       and module Tree.Private.Store = Store
       and type Tree.Local.t = Store.tree
end
