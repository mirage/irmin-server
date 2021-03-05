open Lwt.Syntax
open Lwt.Infix

module Make (Store : Irmin_pack_layered.S) = struct
  include Context.Make (Store)

  module Find = struct
    type req = Store.key

    type res = Store.contents option

    let args = (1, 1)

    let name = "store.find"

    module Server = struct
      let recv _ctx args = Args.next args Store.Key.t

      let handle conn ctx key =
        let* x = Store.find ctx.store key in
        Return.v conn (Irmin.Type.option Store.contents_t) x
    end

    module Client = struct
      let send t key = Args.write t Store.Key.t key

      let recv args = Args.next args (Irmin.Type.option Store.contents_t)
    end
  end

  module Set = struct
    type req = Store.key * Irmin.Info.t * Store.contents

    type res = unit

    let args = (3, 0)

    let name = "store.set"

    module Server = struct
      let recv _ctx args =
        let* key =
          Args.next args Store.Key.t >|= Error.unwrap "store.set key"
        in
        let* info =
          Args.next args Irmin.Info.t >|= Error.unwrap "store.set info"
        in
        let* contents =
          Args.next args Store.Contents.t >|= Error.unwrap "store.set contents"
        in
        Lwt.return_ok (key, info, contents)

      let handle conn ctx (key, info, contents) =
        let* () = Store.set_exn ctx.store key ~info:(fun () -> info) contents in
        Return.ok conn
    end

    module Client = struct
      let send t (key, info, contents) =
        let* () = Args.write t Store.Key.t key in
        let* () = Args.write t Irmin.Info.t info in
        Args.write t Store.contents_t contents

      let recv _args = Lwt.return_ok ()
    end
  end

  module Test_and_set = struct
    type req =
      Store.key * Irmin.Info.t * Store.contents option * Store.contents option

    type res = unit

    let args = (4, 0)

    let name = "store.test_and_set"

    module Server = struct
      let recv _ctx args =
        let* key =
          Args.next args Store.Key.t >|= Error.unwrap "store.test_and_set key"
        in
        let* info =
          Args.next args Irmin.Info.t >|= Error.unwrap "store.test_and_set info"
        in
        let* test =
          Args.next args (Irmin.Type.option Store.Contents.t)
          >|= Error.unwrap "store.test_and_set test"
        in

        let* set =
          Args.next args (Irmin.Type.option Store.Contents.t)
          >|= Error.unwrap "store.test_and_set set"
        in
        Lwt.return_ok (key, info, test, set)

      let handle conn ctx (key, info, test, set) =
        let* () =
          Store.test_and_set_exn ctx.store key ~info:(fun () -> info) ~test ~set
        in
        Return.ok conn
    end

    module Client = struct
      let send t (key, info, test, set) =
        let* () = Args.write t Store.Key.t key in
        let* () = Args.write t Irmin.Info.t info in
        let* () = Args.write t (Irmin.Type.option Store.contents_t) test in
        Args.write t (Irmin.Type.option Store.contents_t) set

      let recv _args = Lwt.return_ok ()
    end
  end

  module Remove = struct
    type req = Store.key * Irmin.Info.t

    type res = unit

    let args = (2, 0)

    let name = "store.remove"

    module Server = struct
      let recv _ctx args =
        let* key =
          Args.next args Store.Key.t >|= Error.unwrap "store.remove key"
        in
        let* info =
          Args.next args Irmin.Info.t >|= Error.unwrap "store.remove info"
        in
        Lwt.return_ok (key, info)

      let handle conn ctx (key, info) =
        let* () = Store.remove_exn ctx.store key ~info:(fun () -> info) in
        Return.ok conn
    end

    module Client = struct
      let send t (key, info) =
        let* () = Args.write t Store.Key.t key in
        Args.write t Irmin.Info.t info

      let recv _args = Lwt.return_ok ()
    end
  end

  module Find_tree = struct
    type req = Store.key

    type res = Tree.t option

    let args = (1, 1)

    let name = "store.find_tree"

    module Server = struct
      let recv _ctx args = Args.next args Store.Key.t

      let handle conn ctx key =
        let* x = Store.find_tree ctx.store key in
        let x = Option.map (fun x -> Tree.Hash (Store.Tree.hash x)) x in
        Return.v conn (Irmin.Type.option Tree.t) x
    end

    module Client = struct
      let send t key = Args.write t Store.Key.t key

      let recv args = Args.next args (Irmin.Type.option Tree.t)
    end
  end

  module Set_tree = struct
    type req = Store.key * Irmin.Info.t * Tree.t

    type res = unit

    let args = (3, 0)

    let name = "store.set_tree"

    module Server = struct
      let recv _ctx args =
        let* key =
          Args.next args Store.Key.t >|= Error.unwrap "store.set_tree key"
        in
        let* info =
          Args.next args Irmin.Info.t >|= Error.unwrap "store.set_tree info"
        in
        let* tree =
          Args.next args Tree.t >|= Error.unwrap "store.set_tree tree"
        in
        Lwt.return_ok (key, info, tree)

      let handle conn ctx (key, info, tree) =
        let* id, tree = resolve_tree ctx tree in
        let* () =
          Store.set_tree_exn ctx.store key ~info:(fun () -> info) tree
        in
        Hashtbl.remove ctx.trees id;
        Return.ok conn
    end

    module Client = struct
      let send t (key, info, tree) =
        let* () = Args.write t Store.Key.t key in
        let* () = Args.write t Irmin.Info.t info in
        Args.write t Tree.t tree

      let recv _args = Lwt.return_ok ()
    end
  end

  module Test_and_set_tree = struct
    type req = Store.key * Irmin.Info.t * Tree.t option * Tree.t option

    type res = unit

    let args = (4, 0)

    let name = "store.test_and_set_tree"

    module Server = struct
      let recv _ctx args =
        let* key =
          Args.next args Store.Key.t
          >|= Error.unwrap "store.test_and_set_tree key"
        in
        let* info =
          Args.next args Irmin.Info.t
          >|= Error.unwrap "store.test_and_set_tree info"
        in
        let* test =
          Args.next args (Irmin.Type.option Tree.t)
          >|= Error.unwrap "store.test_and_set_tree test"
        in

        let* set =
          Args.next args (Irmin.Type.option Tree.t)
          >|= Error.unwrap "store.test_and_set_tree set"
        in
        Lwt.return_ok (key, info, test, set)

      let handle conn ctx ((key, info, test, set) : req) =
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
              (Some id, Some set)
          | None -> Lwt.return (None, None)
        in
        let* () =
          Store.test_and_set_tree_exn ctx.store key
            ~info:(fun () -> info)
            ~test ~set
        in
        Option.iter (Hashtbl.remove ctx.trees) id;
        Return.ok conn
    end

    module Client = struct
      let send t (key, info, test, set) =
        let* () = Args.write t Store.Key.t key in
        let* () = Args.write t Irmin.Info.t info in
        let* () = Args.write t (Irmin.Type.option Tree.t) test in
        Args.write t (Irmin.Type.option Tree.t) set

      let recv _args = Lwt.return_ok ()
    end
  end

  module Mem = struct
    type req = Store.key

    type res = bool

    let args = (1, 1)

    let name = "store.mem"

    module Server = struct
      let recv _ctx args = Args.next args Store.Key.t

      let handle conn ctx key =
        let* res = Store.mem ctx.store key in
        Return.v conn Irmin.Type.bool res
    end

    module Client = struct
      let send t key = Args.write t Store.Key.t key

      let recv args = Args.next args Irmin.Type.bool
    end
  end

  module Mem_tree = struct
    type req = Store.key

    type res = bool

    let args = (1, 1)

    let name = "store.mem_tree"

    module Server = struct
      let recv _ctx args = Args.next args Store.Key.t

      let handle conn ctx key =
        let* res = Store.mem_tree ctx.store key in
        Return.v conn Irmin.Type.bool res
    end

    module Client = struct
      let send t key = Args.write t Store.Key.t key

      let recv args = Args.next args Irmin.Type.bool
    end
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
