open Lwt.Syntax

module Make (Store : Irmin_pack_layered.S with type key = string list) = struct
  include Context.Make (Store)

  module Empty = struct
    module Req = struct
      type t = unit [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "tree.empty"

    let run conn ctx () =
      let empty = Store.Tree.empty in
      let id = Random.bits () in
      Hashtbl.replace ctx.trees id empty;
      Return.v conn Tree.t (ID id)
  end

  module Add = struct
    module Req = struct
      type t = Tree.t * Store.key * Store.contents [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "tree.add"

    let run conn ctx (tree, key, value) =
      let* id, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.add tree key value in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn Tree.t (ID id)
  end

  module Add_tree = struct
    module Req = struct
      type t = Tree.t * Store.key * Tree.t [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "tree.add_tree"

    let run conn ctx (tree, key, tr) =
      let* id, tree = resolve_tree ctx tree in
      let* _, tree' = resolve_tree ctx tr in
      let* tree = Store.Tree.add_tree tree key tree' in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn Tree.t (ID id)
  end

  module Find = struct
    module Req = struct
      type t = Tree.t * Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = Store.contents option [@@deriving irmin]
    end

    let name = "tree.find"

    let run conn ctx (tree, key) =
      let* _, tree = resolve_tree ctx tree in
      let* contents = Store.Tree.find tree key in
      Return.v conn (Irmin.Type.option Store.contents_t) contents
  end

  module Find_tree = struct
    module Req = struct
      type t = Tree.t * Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t option [@@deriving irmin]
    end

    let name = "tree.find_tree"

    let run conn ctx (tree, key) =
      let* _, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.find_tree tree key in
      let tree =
        Option.map
          (fun tree ->
            let id = Random.bits () in
            Hashtbl.replace ctx.trees id tree;
            Tree.ID id)
          tree
      in
      Return.v conn (Irmin.Type.option Tree.t) tree
  end

  module Remove = struct
    module Req = struct
      type t = Tree.t * Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "tree.remove"

    let run conn ctx (tree, key) =
      let* id, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.remove tree key in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn Tree.t (ID id)
  end

  module Abort = struct
    module Req = struct
      type t = Tree.t [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "tree.abort"

    let run conn ctx tree =
      let () =
        match tree with Tree.ID id -> Hashtbl.remove ctx.trees id | _ -> ()
      in
      Return.ok conn
  end

  module Clone = struct
    module Req = struct
      type t = Tree.t [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "tree.clone"

    let run conn ctx tree =
      let* _, tree = resolve_tree ctx tree in
      let id = Random.bits () in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn Tree.t (Tree.ID id)
  end

  module To_local = struct
    module Req = struct
      type t = Tree.t [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.Local.t [@@deriving irmin]
    end

    let name = "tree.to_local"

    let run conn ctx tree =
      let* _, tree = resolve_tree ctx tree in
      Return.v conn Tree.Local.t tree
  end

  module Mem = struct
    module Req = struct
      type t = Tree.t * Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = bool [@@deriving irmin]
    end

    let name = "tree.mem"

    let run conn ctx (tree, key) =
      let* _, tree = resolve_tree ctx tree in
      let* res = Store.Tree.mem tree key in
      Return.v conn Irmin.Type.bool res
  end

  module Mem_tree = struct
    module Req = struct
      type t = Tree.t * Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = bool [@@deriving irmin]
    end

    let name = "tree.mem_tree"

    let run conn ctx (tree, key) =
      let* _, tree = resolve_tree ctx tree in
      let* res = Store.Tree.mem_tree tree key in
      Return.v conn Irmin.Type.bool res
  end

  module List = struct
    module Req = struct
      type t = Tree.t * Store.key [@@deriving irmin]
    end

    type tree = [ `Contents | `Tree ] [@@deriving irmin]

    module Res = struct
      type t = (Store.Key.step * [ `Contents | `Tree ]) list [@@deriving irmin]
    end

    let args = (2, 1)

    let name = "tree.list"

    let run conn ctx (tree, key) =
      let* _, tree = resolve_tree ctx tree in
      let* l = Store.Tree.list tree key in
      let* x =
        Lwt_list.map_s
          (fun (k, _) ->
            let+ exists = Store.Tree.mem_tree tree (Store.Key.rcons key k) in
            if exists then (k, `Tree) else (k, `Contents))
          l
      in
      Return.v conn Irmin.Type.(list (pair Store.Key.step_t tree_t)) x
  end

  let commands =
    [
      cmd (module Empty);
      cmd (module Add);
      cmd (module Remove);
      cmd (module Abort);
      cmd (module Mem);
      cmd (module Mem_tree);
      cmd (module List);
      cmd (module Clone);
      cmd (module To_local);
      cmd (module Find);
      cmd (module Find_tree);
      cmd (module Add_tree);
    ]
end
