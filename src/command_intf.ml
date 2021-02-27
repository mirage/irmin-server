module type S = sig
  module Store : Irmin_pack_layered.S

  module Tree : Tree.S with module Private.Store = Store

  type context = {
    conn : Conn.t;
    repo : Store.Repo.t;
    mutable store : Store.t;
    trees : (int, Store.tree) Hashtbl.t;
  }

  type f = Conn.t -> context -> Args.t -> unit Lwt.t

  module type CMD = sig
    type req

    type res

    val args : int * int

    val name : string

    module Server : sig
      val recv : context -> Args.t -> req Error.result Lwt.t

      val handle : Conn.t -> context -> req -> res Return.t Lwt.t
    end

    module Client : sig
      val send : Args.t -> req -> unit Lwt.t

      val recv : Args.t -> res Error.result Lwt.t
    end
  end

  type t = (module CMD)

  val name : t -> string

  val of_name : string -> t

  val n_args : t -> int

  val n_results : t -> int

  val commands : (string * t) list

  module Commands : sig
    module Ping : CMD with type req = unit and type res = unit

    module Set_branch : CMD with type req = Store.branch and type res = unit

    module Store : sig
      module Find :
        CMD with type req = Store.key and type res = Store.contents option

      module Set :
        CMD
          with type req = Store.key * Irmin.Info.t * Store.contents
           and type res = unit

      module Remove :
        CMD with type req = Store.key * Irmin.Info.t and type res = unit

      module Find_tree :
        CMD with type req = Store.key and type res = Tree.t option

      module Set_tree :
        CMD
          with type req = Store.key * Irmin.Info.t * Tree.t
           and type res = Tree.t
    end

    module Tree : sig
      module Empty : CMD with type req = unit and type res = Tree.t

      module Add :
        CMD
          with type req =
                Tree.t * Tree.Private.Store.key * Tree.Private.Store.contents
           and type res = Tree.t

      module Remove :
        CMD
          with type req = Tree.t * Tree.Private.Store.key
           and type res = Tree.t
    end
  end
end

module type Command = sig
  module type S = S

  module Make (Store : Irmin_pack_layered.S) : S with module Store = Store
end
