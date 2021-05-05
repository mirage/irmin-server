module type S = sig
  module Store : Irmin.S

  module Tree :
    Tree.S with module Private.Store = Store and type Local.t = Store.tree

  module Commit :
    Commit.S
      with type hash = Store.Hash.t
       and type tree = Tree.t
       and module Info = Store.Info

  module Server_info : sig
    type t = { start_time : float }
  end

  type context = {
    conn : Conn.t;
    repo : Store.Repo.t;
    mutable branch : Store.branch;
    mutable store : Store.t;
    trees : (int, Store.tree) Hashtbl.t;
    mutable watch : Store.watch option;
  }

  module Stats : sig
    type t = Stats.t

    val t : t Irmin.Type.t

    val v : Store.repo -> Server_info.t -> t Lwt.t

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

  val name : t -> string

  val of_name : string -> t

  val commands : (string * t) list

  module Commands : sig
    module Stats : CMD with type Req.t = unit and type Res.t = Stats.t

    module Ping : CMD with type Req.t = unit and type Res.t = unit

    (* Branch *)
    module Set_current_branch :
      CMD with type Req.t = Store.branch and type Res.t = unit

    module Get_current_branch :
      CMD with type Req.t = unit and type Res.t = Store.branch

    module Export : CMD with type Req.t = unit and type Res.t = Store.slice

    module Import : CMD with type Req.t = Store.slice and type Res.t = unit

    (* Branch *)
    module Branch_head :
      CMD with type Req.t = Store.branch option and type Res.t = Commit.t option

    module Branch_set_head :
      CMD with type Req.t = Store.branch option * Commit.t and type Res.t = unit

    module Branch_remove :
      CMD with type Req.t = Store.branch and type Res.t = unit

    (* Commit *)
    module Commit_v :
      CMD
        with type Req.t = Store.Info.t * Store.hash list * Tree.t
         and type Res.t = Commit.t

    module Commit_of_hash :
      CMD with type Req.t = Store.Hash.t and type Res.t = Commit.t option

    (* Contents *)
    module Contents_of_hash :
      CMD with type Req.t = Store.Hash.t and type Res.t = Store.contents option

    module Contents_exists :
      CMD with type Req.t = Store.Hash.t and type Res.t = bool

    module Contents_save :
      CMD with type Req.t = Store.contents and type Res.t = Store.Hash.t

    (* Store *)
    module Store : sig
      module Find :
        CMD with type Req.t = Store.key and type Res.t = Store.contents option

      module Set :
        CMD
          with type Req.t = Store.key * Store.Info.t * Store.contents
           and type Res.t = unit

      module Test_and_set :
        CMD
          with type Req.t =
                Store.key
                * Store.Info.t
                * (Store.contents option * Store.contents option)
           and type Res.t = unit

      module Remove :
        CMD with type Req.t = Store.key * Store.Info.t and type Res.t = unit

      module Find_tree :
        CMD with type Req.t = Store.key and type Res.t = Tree.t option

      module Set_tree :
        CMD
          with type Req.t = Store.key * Store.Info.t * Tree.t
           and type Res.t = Tree.t

      module Test_and_set_tree :
        CMD
          with type Req.t =
                Store.key * Store.Info.t * (Tree.t option * Tree.t option)
           and type Res.t = Tree.t option

      module Mem : CMD with type Req.t = Store.key and type Res.t = bool

      module Mem_tree : CMD with type Req.t = Store.key and type Res.t = bool

      module Merge :
        CMD
          with type Req.t = Store.Info.t * Store.Branch.t
           and type Res.t = unit

      module Merge_commit :
        CMD with type Req.t = Store.Info.t * Commit.t and type Res.t = unit

      module Last_modified :
        CMD with type Req.t = Store.key and type Res.t = Commit.t list

      module Watch : CMD with type Req.t = unit and type Res.t = unit

      module Unwatch : CMD with type Req.t = unit and type Res.t = unit
    end

    (* Tree *)
    module Tree : sig
      module Empty : CMD with type Req.t = unit and type Res.t = Tree.t

      module Add :
        CMD
          with type Req.t =
                Tree.t * Tree.Private.Store.key * Tree.Private.Store.contents
           and type Res.t = Tree.t

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

      module Add_tree :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key * Tree.t
           and type Res.t = Tree.t

      module Remove :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = Tree.t

      module Find :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = Tree.Private.Store.contents option

      module Find_tree :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = Tree.t option

      module Cleanup : CMD with type Req.t = Tree.t and type Res.t = unit

      module To_local :
        CMD with type Req.t = Tree.t and type Res.t = Tree.Local.concrete

      module Mem :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = bool

      module Mem_tree :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t = bool

      module List :
        CMD
          with type Req.t = Tree.t * Tree.Private.Store.key
           and type Res.t =
                (Tree.Private.Store.Key.step * [ `Contents | `Tree ]) list

      module Clear : CMD with type Req.t = Tree.t and type Res.t = unit

      module Hash :
        CMD with type Req.t = Tree.t and type Res.t = Tree.Private.Store.Hash.t

      module Cleanup_all : CMD with type Req.t = unit and type Res.t = unit

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
