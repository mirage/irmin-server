open Lwt.Syntax
open Lwt.Infix

module Make
    (IO : Conn.IO)
    (Codec : Conn.Codec.S)
    (Store : Irmin.Generic_key.S)
    (Tree : Tree.S
              with module Private.Store = Store
               and type Local.t = Store.tree)
    (Commit : Commit.S
                with type hash = Store.hash
                 and type tree = Tree.t
                 and type key = Store.commit_key
                 and module Info = Store.Info) =
struct
  include Context.Make (IO) (Codec) (Store) (Tree)
  module Return = Conn.Return

  let convert_commit head =
    let info = Store.Commit.info head in
    let parents = Store.Commit.parents head in
    let key = Store.Commit.key head in
    let tree =
      Tree.Key (Store.Commit.tree head |> Store.Tree.key |> Option.get)
    in
    Commit.v ~info ~parents ~key ~tree

  module Find = struct
    module Req = struct
      type t = Store.path [@@deriving irmin]
    end

    module Res = struct
      type t = Store.contents option [@@deriving irmin]
    end

    let name = "store.find"

    let run conn ctx _ path =
      let* x = Store.find ctx.store path in
      Return.v conn Res.t x
  end

  module Set = struct
    module Req = struct
      type t = Store.path * Store.Info.t * Store.contents [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.set"

    let run conn ctx _ (path, info, contents) =
      let* () = Store.set_exn ctx.store path ~info:(fun () -> info) contents in
      Return.ok conn
  end

  module Test_and_set = struct
    module Req = struct
      type t =
        Store.path
        * Store.Info.t
        * (Store.contents option * Store.contents option)
      [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.test_and_set"

    let run conn ctx _ (path, info, (test, set)) =
      let* () =
        Store.test_and_set_exn ctx.store path ~info:(fun () -> info) ~test ~set
      in
      Return.ok conn
  end

  module Remove = struct
    module Req = struct
      type t = Store.path * Store.Info.t [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.remove"

    let run conn ctx _ (path, info) =
      let* () = Store.remove_exn ctx.store path ~info:(fun () -> info) in
      Return.ok conn
  end

  module Find_tree = struct
    module Req = struct
      type t = Store.path [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t option [@@deriving irmin]
    end

    let name = "store.find_tree"

    let run conn ctx _ path =
      let* x = Store.find_tree ctx.store path in
      let x =
        Option.map (fun x -> Tree.Key (Store.Tree.key x |> Option.get)) x
      in
      Return.v conn Res.t x
  end

  module Set_tree = struct
    module Req = struct
      type t = Store.path * Store.Info.t * Tree.t [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t [@@deriving irmin]
    end

    let name = "store.set_tree"

    let run conn ctx _ (path, info, tree) =
      let* id, tree = resolve_tree ctx tree in
      let* () = Store.set_tree_exn ctx.store path ~info:(fun () -> info) tree in
      let* tree = Store.get_tree ctx.store path in
      let key = Store.Tree.key tree in
      Option.iter (fun id -> Hashtbl.remove ctx.trees id) id;
      Return.v conn Res.t (Tree.Key (Option.get key))
  end

  module Test_and_set_tree = struct
    module Req = struct
      type t = Store.path * Store.Info.t * (Tree.t option * Tree.t option)
      [@@deriving irmin]
    end

    module Res = struct
      type t = Tree.t option [@@deriving irmin]
    end

    let name = "store.test_and_set_tree"

    let run conn ctx _ ((path, info, (test, set)) : Req.t) =
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
        Store.test_and_set_tree_exn ctx.store path
          ~info:(fun () -> info)
          ~test ~set
      in
      Option.iter (Hashtbl.remove ctx.trees) id;
      Return.v conn Res.t
        (Option.map
           (fun tree -> Tree.Key (Store.Tree.key tree |> Option.get))
           set)
  end

  module Mem = struct
    module Req = struct
      type t = Store.path [@@deriving irmin]
    end

    module Res = struct
      type t = bool [@@deriving irmin]
    end

    let name = "store.mem"

    let run conn ctx _ path =
      let* res = Store.mem ctx.store path in
      Return.v conn Res.t res
  end

  module Mem_tree = struct
    module Req = struct
      type t = Store.path [@@deriving irmin]
    end

    module Res = struct
      type t = bool [@@deriving irmin]
    end

    let name = "store.mem_tree"

    let run conn ctx _ path =
      let* res = Store.mem_tree ctx.store path in
      Return.v conn Res.t res
  end

  module Merge = struct
    module Req = struct
      type t = Store.Info.t * Store.Branch.t [@@deriving irmin]
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
      type t = Store.Info.t * Commit.t [@@deriving irmin]
    end

    module Res = struct
      type t = unit [@@deriving irmin]
    end

    let name = "store.merge_commit"

    let run conn ctx _ ((info, other) : Req.t) =
      let* commit =
        Store.Commit.of_key ctx.repo (Commit.key other) >|= Option.get
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
      type t = Store.path [@@deriving irmin]
    end

    module Res = struct
      type t = Commit.t list [@@deriving irmin]
    end

    let name = "store.last_modified"

    let run conn ctx _ path =
      let* res =
        Store.last_modified ctx.store path >|= List.map convert_commit
      in
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
      cmd (module Merge);
      cmd (module Merge_commit);
      cmd (module Last_modified);
    ]
end
