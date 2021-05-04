open Lwt.Syntax
open Lwt.Infix

module Make
    (Store : Irmin.S)
    (Tree : Tree.S
              with module Private.Store = Store
               and type Local.t = Store.tree)
    (Commit : Commit.S with type hash = Store.hash and type tree = Tree.t) =
struct
  include Context.Make (Store) (Tree)

  let convert_commit head =
    let info = Store.Commit.info head in
    let parents = Store.Commit.parents head in
    let hash = Store.Commit.hash head in
    let tree = Tree.Hash (Store.Commit.tree head |> Store.Tree.hash) in
    Commit.v ~info ~parents ~hash ~tree

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

  module Merge = struct
    module Req = struct
      type t = Irmin.Info.t * Store.Branch.t [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.merge"

    let run conn ctx _ (info, other) =
      let* merge =
        Store.merge_with_branch ctx.store other ~info:(fun () -> info)
      in
      match merge with
      | Ok () -> Return.v conn Res.t ()
      | Error e ->
          Return.err conn (Irmin.Type.to_string Irmin.Merge.conflict_t e)
  end

  module Merge_commit = struct
    module Req = struct
      type t = Irmin.Info.t * Commit.t [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.merge_commit"

    let run conn ctx _ ((info, other) : Req.t) =
      let* commit =
        Store.Commit.of_hash ctx.repo (Commit.hash other) >|= Option.get
      in
      let* merge =
        Store.merge_with_commit ctx.store commit ~info:(fun () -> info)
      in
      match merge with
      | Ok () -> Return.v conn Res.t ()
      | Error e ->
          Return.err conn (Irmin.Type.to_string Irmin.Merge.conflict_t e)
  end

  module Last_modified = struct
    module Req = struct
      type t = Store.key [@@deriving irmin]
    end

    module Res = struct
      type t = Commit.t list [@@deriving irmin]
    end

    let name = "store.last_modified"

    let run conn ctx _ key =
      let* res =
        Store.last_modified ctx.store key >|= List.map convert_commit
      in
      Return.v conn Res.t res
  end

  module Watch = struct
    module Req = struct
      type t = unit [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.watch"

    let run conn ctx _ () =
      let* () =
        match ctx.watch with
        | Some watch ->
            ctx.watch <- None;
            Store.unwatch watch
        | None -> Lwt.return_unit
      in
      let* watch =
        Store.watch ctx.store (fun diff ->
            let diff =
              match diff with
              | `Updated (a, b) -> `Updated (convert_commit a, convert_commit b)
              | `Added a -> `Added (convert_commit a)
              | `Removed a -> `Removed (convert_commit a)
            in
            Conn.write_message conn (Irmin.Diff.t Commit.t) diff)
      in
      ctx.watch <- Some watch;
      Return.v conn Res.t ()
  end

  module Unwatch = struct
    module Req = struct
      type t = unit [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.unwatch"

    let run conn ctx _ () =
      let* () =
        match ctx.watch with
        | Some watch ->
            ctx.watch <- None;
            Store.unwatch watch
        | None -> Lwt.return_unit
      in
      Return.v conn Res.t ()
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
      cmd (module Merge);
      cmd (module Merge_commit);
      cmd (module Last_modified);
      cmd (module Watch);
      cmd (module Unwatch);
    ]
end
