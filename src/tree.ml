open Lwt.Syntax
include Tree_intf

module Make (Store : Irmin_pack_layered.S) = struct
  module Private = struct
    module Store = Store
  end

  module Local = struct
    type t = Store.tree

    let t = Store.tree_t

    let empty = Store.Tree.empty

    let list = Store.Tree.list

    let of_contents x = Store.Tree.of_contents x

    let add t key contents = Store.Tree.add t key contents

    let add_tree t key tree = Store.Tree.add_tree t key tree

    let mem = Store.Tree.mem

    let mem_tree = Store.Tree.mem_tree

    let update = Store.Tree.update ~metadata:Store.Metadata.default

    let update_tree = Store.Tree.update_tree

    let remove = Store.Tree.remove

    let find = Store.Tree.find

    let find_tree = Store.Tree.find_tree

    let destruct x =
      match Store.Tree.destruct x with
      | `Contents (x, _) -> Lwt.return (`Contents (Store.Tree.Contents.hash x))
      | `Node l ->
          let+ l = list (Store.Tree.of_node l) Store.Key.empty in
          `Node l

    let kind = Store.Tree.kind
  end

  type t = Hash of Store.Hash.t | ID of int | Local of Local.t
  [@@deriving irmin]
end
