open Lwt.Syntax
open Lwt.Infix

module Make (Store : Irmin_pack_layered.S) = struct
  include Context.Make (Store)

  module Empty = struct
    type req = unit

    type res = Tree.t

    let name = "tree.empty"

    let args = (0, 1)

    module Server = struct
      let recv _ctx _args = Lwt.return_ok ()

      let handle conn ctx () =
        let empty = Store.Tree.empty in
        let id = Random.bits () in
        Hashtbl.replace ctx.trees id empty;
        Return.v conn Tree.t (ID id)
    end

    module Client = struct
      let send _args () = Lwt.return_unit

      let recv args = Args.next args Tree.t
    end
  end

  module Add = struct
    type req = Tree.t * Store.key * Store.contents

    type res = Tree.t

    let name = "tree.add"

    let args = (3, 1)

    module Server = struct
      let recv _ctx args =
        let* tree = Args.next args Tree.t >|= Error.unwrap "tree.add tree" in
        let* key = Args.next args Store.key_t >|= Error.unwrap "tree.add key" in
        let* value =
          Args.next args Store.contents_t >|= Error.unwrap "tree.add value"
        in
        Lwt.return_ok (tree, key, value)

      let handle conn ctx (tree, key, value) =
        let* id, tree = resolve_tree ctx tree in
        let* tree = Store.Tree.add tree key value in
        Hashtbl.replace ctx.trees id tree;
        Return.v conn Tree.t (ID id)
    end

    module Client = struct
      let send t (tree, key, contents) =
        let* () = Args.write t Tree.t tree in
        let* () = Args.write t Store.key_t key in
        Args.write t Store.contents_t contents

      let recv args = Args.next args Tree.t
    end
  end

  module Remove = struct
    type req = Tree.t * Store.key

    type res = Tree.t

    let name = "tree.remove"

    let args = (2, 1)

    module Server = struct
      let recv _ctx args =
        let* tree = Args.next args Tree.t >|= Error.unwrap "tree.remove tree" in
        let* key =
          Args.next args Store.key_t >|= Error.unwrap "tree.remove key"
        in
        Lwt.return_ok (tree, key)

      let handle conn ctx (tree, key) =
        let* id, tree = resolve_tree ctx tree in
        let* tree = Store.Tree.remove tree key in
        Hashtbl.replace ctx.trees id tree;
        Return.v conn Tree.t (ID id)
    end

    module Client = struct
      let send t (tree, key) =
        let* () = Args.write t Tree.t tree in
        Args.write t Store.key_t key

      let recv args = Args.next args Tree.t
    end
  end

  let commands = [ cmd (module Empty); cmd (module Add); cmd (module Remove) ]
end
