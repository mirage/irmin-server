open Lwt.Syntax
include Tree_intf

module Make (Store : Irmin.S) = struct
  module Private = struct
    module Store = Store
  end

  module Local = struct
    type t = Private.Store.tree

    type key = Private.Store.key

    type contents = Private.Store.contents

    type node = Private.Store.node

    type hash = Private.Store.hash

    type step = Private.Store.Key.step

    type metadata = Private.Store.metadata

    include Private.Store.Tree

    let t = Private.Store.tree_t

    let destruct x =
      match Private.Store.Tree.destruct x with
      | `Contents (x, _) ->
          Lwt.return (`Contents (Private.Store.Tree.Contents.hash x))
      | `Node l ->
          let+ l =
            list (Private.Store.Tree.of_node l) Private.Store.Key.empty
          in
          `Node l
  end

  type t = Hash of Private.Store.Hash.t | ID of int | Local of Local.concrete
  [@@deriving irmin]
end
