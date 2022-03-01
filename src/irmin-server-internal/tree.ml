open Lwt.Syntax
include Tree_intf

module Make (Store : Irmin.Generic_key.S) = struct
  module Private = struct
    module Store = Store
  end

  module Local = struct
    type t = Private.Store.tree
    type path = Private.Store.path
    type contents = Private.Store.contents
    type node = Private.Store.node
    type hash = Private.Store.hash
    type step = Private.Store.Path.step
    type metadata = Private.Store.metadata

    include Private.Store.Tree

    let t = Store.tree_t

    let destruct x =
      match Store.Tree.destruct x with
      | `Contents (x, _) -> Lwt.return (`Contents (Store.Tree.Contents.hash x))
      | `Node l ->
          let+ l = list (Store.Tree.of_node l) Store.Path.empty in
          `Node l
  end

  type t =
    | Key of Private.Store.Tree.kinded_key
    | ID of int
    | Local of Local.concrete
  [@@deriving irmin]
end
