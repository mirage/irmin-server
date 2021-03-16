open Lwt.Syntax
include Tree_intf

module Make (Store : Irmin_pack_layered.S) = struct
  module Private = struct
    module Store = Store
  end

  module Local = struct
    type t = Store.tree

    let t = Store.tree_t

    type key = Store.key

    type contents = Store.contents

    type node = Store.node

    type hash = Store.hash

    type step = Store.Key.step

    type metadata = Store.metadata

    include Store.Tree

    let destruct x =
      match Store.Tree.destruct x with
      | `Contents (x, _) -> Lwt.return (`Contents (Store.Tree.Contents.hash x))
      | `Node l ->
          let+ l = list (Store.Tree.of_node l) Store.Key.empty in
          `Node l
  end

  type t = Hash of Store.Hash.t | ID of int | Local of Local.t
  [@@deriving irmin]
end
