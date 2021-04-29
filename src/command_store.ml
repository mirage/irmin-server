open Lwt.Syntax

module Make (Store : Command_intf.STORE) = struct
  include Context.Make (Store)

  module Find = struct
    module Req = struct
      type t = Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = Store.contents option [@@deriving irmin]
    end

    let name = "store.find"

    let run conn ctx _ key =
      let* x = Store.find ctx.store key in
      Return.v conn Res.t x
  end

  module Set = struct
    module Req = struct
      type t = Store.key * Irmin.Info.t * Store.contents [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.set"

    let run conn ctx _ (key, info, contents) =
      let* () = Store.set_exn ctx.store key ~info:(fun () -> info) contents in
      Return.ok conn
  end

  module Test_and_set = struct
    module Req = struct
      type t =
        Store.key
        * Irmin.Info.t
        * (Store.contents option * Store.contents option)
      [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.test_and_set"

    let run conn ctx _ (key, info, (test, set)) =
      let* () =
        Store.test_and_set_exn ctx.store key ~info:(fun () -> info) ~test ~set
      in
      Return.ok conn
  end

  module Remove = struct
    module Req = struct
      type t = Store.key * Irmin.Info.t [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.remove"

    let run conn ctx _ (key, info) =
      let* () = Store.remove_exn ctx.store key ~info:(fun () -> info) in
      Return.ok conn
  end

  module Find_tree = struct
    module Req = struct
      type t = Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t option [@@deriving irmin]
    end

    let name = "store.find_tree"

    let run conn ctx _ key =
      let* x = Store.find_tree ctx.store key in
      let x = Option.map (fun x -> Tree.Hash (Store.Tree.hash x)) x in
      Return.v conn Res.t x
  end

  module Set_tree = struct
    module Req = struct
      type t = Store.key * Irmin.Info.t * Tree.t [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "store.set_tree"

    let run conn ctx _ (key, info, tree) =
      let* id, tree = resolve_tree ctx tree in
      let* () = Store.set_tree_exn ctx.store key ~info:(fun () -> info) tree in
      Option.iter (fun id -> Hashtbl.remove ctx.trees id) id;
      Return.v conn Res.t (Tree.Hash (Store.Tree.hash tree))
  end

  module Test_and_set_tree = struct
    module Req = struct
      type t = Store.key * Irmin.Info.t * (Tree.t option * Tree.t option)
      [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t option [@@deriving irmin]
    end

    let name = "store.test_and_set_tree"

    let run conn ctx _ ((key, info, (test, set)) : Req.t) =
      let* test =
        match test with
        | Some test ->
            let+ _, test = resolve_tree ctx test in
            Some test
        | None -> Lwt.return_none
      in
      let* id, set =
        match set with
        | Some set ->
            let+ id, set = resolve_tree ctx set in
            (id, Some set)
        | None -> Lwt.return (None, None)
      in
      let* () =
        Store.test_and_set_tree_exn ctx.store key
          ~info:(fun () -> info)
          ~test ~set
      in
      Option.iter (Hashtbl.remove ctx.trees) id;
      Return.v conn Res.t
        (Option.map (fun tree -> Tree.Hash (Store.Tree.hash tree)) set)
  end

  module Mem = struct
    module Req = struct
      type t = Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = bool [@@deriving irmin]
    end

    let name = "store.mem"

    let run conn ctx _ key =
      let* res = Store.mem ctx.store key in
      Return.v conn Res.t res
  end

  module Mem_tree = struct
    module Req = struct
      type t = Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = bool [@@deriving irmin]
    end

    let name = "store.mem_tree"

    let run conn ctx _ key =
      let* res = Store.mem_tree ctx.store key in
      Return.v conn Res.t res
  end

  let commands =
    [
      cmd (module Find);
      cmd (module Set);
      cmd (module Remove);
      cmd (module Find_tree);
      cmd (module Set_tree);
      cmd (module Mem);
      cmd (module Mem_tree);
      cmd (module Test_and_set);
      cmd (module Test_and_set_tree);
    ]
end
