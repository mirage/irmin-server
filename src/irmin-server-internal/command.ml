open Lwt.Syntax
open Lwt.Infix
include Command_intf

module Make (IO : Conn.IO) (Codec : Conn.Codec.S) (St : Irmin.Generic_key.S) =
struct
  module Store = St
  module Tree = Tree.Make (St)
  module Commit = Commit.Make (St) (Tree)
  include Context.Make (IO) (Codec) (St) (Tree)
  module Return = Conn.Return

  type t = (module CMD)

  let convert_commit head =
    let info = Store.Commit.info head in
    let parents = Store.Commit.parents head in
    let key = Store.Commit.key head in
    let tree =
      Tree.Key (Store.Commit.tree head |> Store.Tree.key |> Option.get)
    in
    Commit.v ~info ~parents ~key ~tree

  module Stats = struct
    type t = Stats.t

    let t = Stats.t

    let file f =
      try float_of_int (Unix.stat f).st_size
      with Unix.Unix_error (Unix.ENOENT, _, _) -> 0.

    let dict root = file (Irmin_pack.Layout.dict ~root) /. 1024. /. 1024.
    let pack root = file (Irmin_pack.Layout.pack ~root) /. 1024. /. 1024.

    let index root =
      let index_dir = Filename.concat root "index" in
      let a = file (Filename.concat index_dir "data") in
      let b = file (Filename.concat index_dir "log") in
      let c = file (Filename.concat index_dir "log_async") in
      (a +. b +. c) /. 1024. /. 1024.

    let size root = dict root +. pack root +. index root

    let v ctx info : t Lwt.t =
      let pack = Irmin_pack.Stats.get () in
      let uptime = Server_info.uptime info in
      let* branches =
        St.Branch.list ctx.repo
        >>= Lwt_list.map_s (fun b ->
                let+ head = St.Branch.find ctx.repo b in
                let hash =
                  Option.map
                    (fun c -> Fmt.to_to_string St.Commit.pp_hash c)
                    head
                in
                (Irmin.Type.to_string St.Branch.t b, hash))
      in
      let root = Irmin_pack.Conf.root ctx.config in
      let cache_stats = Irmin_pack.Stats.get_cache_stats () in
      Lwt.return
        Stats.
          {
            uptime;
            branches;
            finds = pack.finds;
            cache_misses = cache_stats.cache_misses;
            adds = pack.appended_hashes + pack.appended_offsets;
            size = size root;
          }

    let to_json = Irmin.Type.to_json_string t
  end

  module Commands = struct
    module Backend = struct
      module Schema = Store.Schema
      module Hash = Store.Hash

      module Contents = struct
        type key = Store.Backend.Contents.key

        let key_t = Store.Backend.Contents.Key.t

        type value = Store.Backend.Contents.value

        let value_t = Store.Backend.Contents.Val.t

        type hash = Store.Backend.Contents.hash

        let hash_t = Store.Backend.Contents.Hash.t

        module Mem = struct
          let name = "x.contents.mem"

          type req = key [@@deriving irmin]
          type res = bool [@@deriving irmin]

          let run conn ctx _ key =
            let* x =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  St.Backend.Contents.mem contents key)
            in
            Return.v conn res_t x
        end

        module Find = struct
          let name = "x.contents.find"

          type req = key [@@deriving irmin]
          type res = value option [@@deriving irmin]

          let run conn ctx _ key =
            let* v =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  St.Backend.Contents.find contents key)
            in
            Return.v conn res_t v
        end

        module Add = struct
          let name = "x.contents.add"

          type req = value [@@deriving irmin]
          type res = key [@@deriving irmin]

          let run conn ctx _ value =
            let* k =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  St.Backend.Contents.add contents value)
            in
            Return.v conn res_t k
        end

        module Unsafe_add = struct
          let name = "x.contents.unsafe_add"

          type req = hash * value [@@deriving irmin]
          type res = key [@@deriving irmin]

          let run conn ctx _ (hash, value) =
            let* k =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  St.Backend.Contents.unsafe_add contents hash value)
            in
            Return.v conn res_t k
        end

        module Index = struct
          let name = "x.contents.index"

          type req = hash [@@deriving irmin]
          type res = key option [@@deriving irmin]

          let run conn ctx _ hash =
            let* v =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  St.Backend.Contents.index contents hash)
            in
            Return.v conn res_t v
        end

        module Clear = struct
          let name = "x.contents.clear"

          type req = unit [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let run conn ctx _ () =
            let* () =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  St.Backend.Contents.clear contents)
            in
            Return.v conn res_t ()
        end

        module Merge = struct
          let name = "x.contents.merge"

          type req = key option option * key option * key option
          [@@deriving irmin]

          type res = (key option, Irmin.Merge.conflict) Result.t
          [@@deriving irmin]

          let run conn ctx _ (old, a, b) =
            let* res =
              St.Backend.Repo.batch ctx.repo (fun contents _ _ ->
                  let merge = St.Backend.Contents.merge contents in
                  let f = Irmin.Merge.f merge in
                  let old () = Lwt.return_ok old in
                  f ~old a b)
            in
            Return.v conn res_t res
        end
      end

      module Node = struct
        type key = Store.Backend.Node.key

        let key_t = Store.Backend.Node.Key.t

        type value = Store.Backend.Node.value

        let value_t = Store.Backend.Node.Val.t

        type hash = Hash.t

        module Mem = struct
          let name = "x.node.mem"

          type req = key [@@deriving irmin]
          type res = bool [@@deriving irmin]

          let run conn ctx _ key =
            let* x =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  St.Backend.Node.mem node key)
            in
            Return.v conn res_t x
        end

        module Find = struct
          let name = "x.node.find"

          type req = key [@@deriving irmin]
          type res = value option [@@deriving irmin]

          let run conn ctx _ key =
            let* v =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  St.Backend.Node.find node key)
            in
            Return.v conn res_t v
        end

        module Add = struct
          let name = "x.node.add"

          type req = value [@@deriving irmin]
          type res = key [@@deriving irmin]

          let run conn ctx _ value =
            let* k =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  St.Backend.Node.add node value)
            in
            Return.v conn res_t k
        end

        module Unsafe_add = struct
          let name = "x.node.unsafe_add"

          type req = Hash.t * value [@@deriving irmin]
          type res = key [@@deriving irmin]

          let run conn ctx _ (hash, value) =
            let* k =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  St.Backend.Node.unsafe_add node hash value)
            in
            Return.v conn res_t k
        end

        module Index = struct
          let name = "x.node.index"

          type req = Hash.t [@@deriving irmin]
          type res = key option [@@deriving irmin]

          let run conn ctx _ hash =
            let* v =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  St.Backend.Node.index node hash)
            in
            Return.v conn res_t v
        end

        module Clear = struct
          let name = "x.node.clear"

          type req = unit [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let run conn ctx _ () =
            let* () =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  St.Backend.Node.clear node)
            in
            Return.v conn res_t ()
        end

        module Merge = struct
          let name = "x.node.merge"

          type req = key option option * key option * key option
          [@@deriving irmin]

          type res = (key option, Irmin.Merge.conflict) Result.t
          [@@deriving irmin]

          let run conn ctx _ (old, a, b) =
            let* res =
              St.Backend.Repo.batch ctx.repo (fun _ node _ ->
                  let merge = St.Backend.Node.merge node in
                  let f = Irmin.Merge.f merge in
                  let old () = Lwt.return_ok old in
                  f ~old a b)
            in
            Return.v conn res_t res
        end
      end

      module Commit = struct
        type key = Store.Backend.Commit.key

        let key_t = Store.Backend.Commit.Key.t

        type value = Store.Backend.Commit.value

        let value_t = Store.Backend.Commit.Val.t

        type hash = Hash.t

        module Mem = struct
          let name = "x.commit.mem"

          type req = key [@@deriving irmin]
          type res = bool [@@deriving irmin]

          let run conn ctx _ key =
            let* x =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  St.Backend.Commit.mem commit key)
            in
            Return.v conn res_t x
        end

        module Find = struct
          let name = "x.commit.find"

          type req = key [@@deriving irmin]
          type res = value option [@@deriving irmin]

          let run conn ctx _ key =
            let* v =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  St.Backend.Commit.find commit key)
            in
            Return.v conn res_t v
        end

        module Add = struct
          let name = "x.commit.add"

          type req = value [@@deriving irmin]
          type res = key [@@deriving irmin]

          let run conn ctx _ value =
            let* k =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  St.Backend.Commit.add commit value)
            in
            Return.v conn res_t k
        end

        module Unsafe_add = struct
          let name = "x.commit.unsafe_add"

          type req = Hash.t * value [@@deriving irmin]
          type res = key [@@deriving irmin]

          let run conn ctx _ (hash, value) =
            let* k =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  St.Backend.Commit.unsafe_add commit hash value)
            in
            Return.v conn res_t k
        end

        module Index = struct
          let name = "x.commit.index"

          type req = Hash.t [@@deriving irmin]
          type res = key option [@@deriving irmin]

          let run conn ctx _ hash =
            let* v =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  St.Backend.Commit.index commit hash)
            in
            Return.v conn res_t v
        end

        module Clear = struct
          let name = "x.commit.clear"

          type req = unit [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let run conn ctx _ () =
            let* () =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  St.Backend.Commit.clear commit)
            in
            Return.v conn res_t ()
        end

        module Merge = struct
          let name = "x.commit.merge"

          type req =
            Store.Info.t * (key option option * key option * key option)
          [@@deriving irmin]

          type res = (key option, Irmin.Merge.conflict) Result.t
          [@@deriving irmin]

          let run conn ctx _ (info, (old, a, b)) =
            let info () = info in
            let* res =
              St.Backend.Repo.batch ctx.repo (fun _ _ commit ->
                  let merge = St.Backend.Commit.merge commit ~info in
                  let f = Irmin.Merge.f merge in
                  let old () = Lwt.return_ok old in
                  f ~old a b)
            in
            Return.v conn res_t res
        end
      end

      module Branch = struct
        type key = Schema.Branch.t [@@deriving irmin]
        type value = Store.commit_key [@@deriving irmin]

        module Mem = struct
          let name = "x.branch.mem"

          type req = key [@@deriving irmin]
          type res = bool [@@deriving irmin]

          let run conn ctx _ branch =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* x = Store.Backend.Branch.mem b branch in
            Return.v conn res_t x
        end

        module Find = struct
          let name = "x.branch.find"

          type req = key [@@deriving irmin]
          type res = value option [@@deriving irmin]

          let run conn ctx _ branch =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* commit = Store.Backend.Branch.find b branch in
            Return.v conn res_t commit
        end

        module Set = struct
          let name = "x.branch.set"

          type req = key * value [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let run conn ctx _ (branch, commit) =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* () = Store.Backend.Branch.set b branch commit in
            Return.v conn res_t ()
        end

        module Test_and_set = struct
          let name = "x.branch.test_and_set"

          type req = key * value option * value option [@@deriving irmin]
          type res = bool [@@deriving irmin]

          let run conn ctx _ (branch, test, set) =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* res = Store.Backend.Branch.test_and_set b branch ~test ~set in
            Return.v conn res_t res
        end

        module Remove = struct
          let name = "x.branch.remove"

          type req = key [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let run conn ctx _ branch =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* () = Store.Backend.Branch.remove b branch in
            Return.v conn res_t ()
        end

        module List = struct
          let name = "x.branch.list"

          type req = unit [@@deriving irmin]
          type res = key list [@@deriving irmin]

          let run conn ctx _ () =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* b = Store.Backend.Branch.list b in
            Return.v conn res_t b
        end

        module Clear = struct
          let name = "x.branch.clear"

          type req = unit [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let run conn ctx _ () =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* () = Store.Backend.Branch.clear b in
            Return.v conn res_t ()
        end

        module Watch = struct
          type req = (key * value) list option [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let name = "x.branch.watch"

          let run conn ctx _ init =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* () =
              match ctx.branch_watch with
              | Some watch ->
                  ctx.branch_watch <- None;
                  Store.Backend.Branch.unwatch b watch
              | None -> Lwt.return_unit
            in
            let* watch =
              Store.Backend.Branch.watch b ?init (fun key diff ->
                  let diff_t = Irmin.Diff.t Store.commit_key_t in
                  Conn.write conn
                    (Irmin.Type.pair Store.Branch.t diff_t)
                    (key, diff))
            in
            ctx.branch_watch <- Some watch;
            Return.v conn res_t ()
        end

        module Watch_key = struct
          type req = value option * key [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let name = "x.branch.watch_key"

          let run conn ctx _ (init, key) =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* () =
              match ctx.branch_watch with
              | Some watch ->
                  ctx.branch_watch <- None;
                  Store.Backend.Branch.unwatch b watch
              | None -> Lwt.return_unit
            in
            let* watch =
              Store.Backend.Branch.watch_key b key ?init (fun diff ->
                  let diff_t = Irmin.Diff.t Store.commit_key_t in
                  Conn.write conn diff_t diff)
            in
            ctx.branch_watch <- Some watch;
            Return.v conn res_t ()
        end

        module Unwatch = struct
          type req = unit [@@deriving irmin]
          type res = unit [@@deriving irmin]

          let name = "x.branch.unwatch"

          let run conn ctx _ () =
            let b = Store.Backend.Repo.branch_t ctx.repo in
            let* () =
              match ctx.branch_watch with
              | Some watch ->
                  ctx.branch_watch <- None;
                  Store.Backend.Branch.unwatch b watch
              | None -> Lwt.return_unit
            in
            Return.v conn res_t ()
        end
      end
    end

    module Stats = struct
      let name = "stats"

      type req = unit [@@deriving irmin]
      type res = Stats.t [@@deriving irmin]

      let run conn ctx info () =
        let* stats = Stats.v ctx info in
        Return.v conn res_t stats
    end

    module Ping = struct
      let name = "ping"

      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn _ctx _ () = Return.ok conn
    end

    module Set_current_branch = struct
      type req = St.Branch.t [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "set_current_branch"

      let run conn ctx _ branch =
        let* store = Store.of_branch ctx.repo branch in
        ctx.branch <- branch;
        ctx.store <- store;
        Return.ok conn
    end

    module Get_current_branch = struct
      type req = unit [@@deriving irmin]
      type res = St.Branch.t [@@deriving irmin]

      let name = "get_current_branch"
      let run conn ctx _ () = Return.v conn Store.Branch.t ctx.branch
    end

    module Export = struct
      type req = int option [@@deriving irmin]
      type res = St.slice [@@deriving irmin]

      let name = "export"

      let run conn ctx _ depth =
        let* slice = Store.Repo.export ?depth ~full:true ~max:`Head ctx.repo in
        Return.v conn Store.slice_t slice
    end

    module Import = struct
      type req = St.slice [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "import"

      let run conn ctx _ slice =
        let* () = Store.Repo.import ctx.repo slice >|= Error.unwrap "import" in
        Return.ok conn
    end

    module Branch_remove = struct
      type req = St.Branch.t [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "branch.remove"

      let run conn ctx _ branch =
        let* () = Store.Branch.remove ctx.repo branch in
        let* () =
          if Irmin.Type.(unstage (equal Store.Branch.t)) ctx.branch branch then
            let+ store = Store.main ctx.repo in
            let () = ctx.branch <- Store.Branch.main in
            ctx.store <- store
          else Lwt.return_unit
        in
        Return.ok conn
    end

    module Branch_head = struct
      type req = St.branch option [@@deriving irmin]
      type res = Commit.t option [@@deriving irmin]

      let name = "branch.head"

      let run conn ctx _ branch =
        let branch = Option.value ~default:ctx.branch branch in
        let* head = Store.Branch.find ctx.repo branch in
        match head with
        | None -> Return.v conn res_t None
        | Some head -> Return.v conn res_t (Some (convert_commit head))
    end

    module Branch_set_head = struct
      type req = St.branch option * Commit.t [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "branch.set_head"

      let run conn ctx _ (branch, commit) =
        let branch = Option.value ~default:ctx.branch branch in
        let* commit =
          St.Commit.of_key ctx.repo (Commit.key commit) >|= Option.get
        in
        let* () = St.Branch.set ctx.repo branch commit in
        Return.ok conn
    end

    module Commit_v = struct
      type req = St.Info.t * St.commit_key list * Tree.t [@@deriving irmin]
      type res = Commit.t [@@deriving irmin]

      let name = "commit.v"

      let run conn ctx _ (info, parents, tree) =
        let* _, tree = resolve_tree ctx tree in
        let* commit = St.Commit.v ctx.repo ~info ~parents tree in
        let key = St.Commit.key commit in
        let tree_ = St.Commit.tree commit in
        let tree =
          Tree.Key (St.Commit.tree commit |> St.Tree.key |> Option.get)
        in
        let head = Commit.v ~info ~parents ~key ~tree in
        St.Tree.clear tree_;
        reset_trees ctx;
        Return.v conn Commit.t head
    end

    module Commit_of_key = struct
      type req = St.commit_key [@@deriving irmin]
      type res = Commit.t option [@@deriving irmin]

      let name = "commit.of_key"

      let run conn ctx _ hash =
        let* commit = St.Commit.of_key ctx.repo hash in
        let commit = Option.map convert_commit commit in
        Return.v conn res_t commit
    end

    module Commit_hash_of_key = struct
      type req = Commit.key [@@deriving irmin]
      type res = St.Hash.t option [@@deriving irmin]

      let name = "commit.hash_of_key"

      let run conn ctx _ key =
        let* commit = St.Commit.of_key ctx.repo key in
        let hash = Option.map St.Commit.hash commit in
        Return.v conn res_t hash
    end

    module Commit_of_hash = struct
      type req = St.hash [@@deriving irmin]
      type res = Commit.t option [@@deriving irmin]

      let name = "commit.of_key"

      let run conn ctx _ hash =
        let* commit = St.Commit.of_hash ctx.repo hash in
        let commit = Option.map convert_commit commit in
        Return.v conn res_t commit
    end

    module Contents_of_hash = struct
      type req = St.hash [@@deriving irmin]
      type res = St.contents option [@@deriving irmin]

      let name = "contents.of_hash"

      let run conn ctx _ key =
        let* contents = St.Contents.of_hash ctx.repo key in
        Return.v conn res_t contents
    end

    module Contents_save = struct
      type req = St.contents [@@deriving irmin]
      type res = St.Hash.t [@@deriving irmin]

      let name = "contents.save"

      let run conn ctx _ contents =
        let* k =
          St.Backend.Repo.batch ctx.repo (fun t _ _ ->
              St.save_contents t contents)
        in
        let* contents = St.Contents.of_key ctx.repo k >|= Option.get in
        let hash = St.Contents.hash contents in
        Return.v conn res_t hash
    end

    module Contents_exists = struct
      type req = St.hash [@@deriving irmin]
      type res = bool [@@deriving irmin]

      let name = "contents.exists"

      let run conn ctx _ hash =
        let* contents = St.Contents.of_hash ctx.repo hash in
        Return.v conn res_t (Option.is_some contents)
    end

    module Watch = struct
      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "watch"

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
                | `Updated (a, b) ->
                    `Updated (convert_commit a, convert_commit b)
                | `Added a -> `Added (convert_commit a)
                | `Removed a -> `Removed (convert_commit a)
              in
              Conn.write conn (Irmin.Diff.t Commit.t) diff)
        in
        ctx.watch <- Some watch;
        Return.v conn res_t ()
    end

    module Unwatch = struct
      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "unwatch"

      let run conn ctx _ () =
        let* () =
          match ctx.watch with
          | Some watch ->
              ctx.watch <- None;
              Store.unwatch watch
          | None -> Lwt.return_unit
        in
        Return.v conn res_t ()
    end

    module Store = Command_store.Make (IO) (Codec) (St) (Tree) (Commit)
    module Tree = Command_tree.Make (IO) (Codec) (St) (Tree) (Commit)
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [
      cmd (module Backend.Contents.Mem);
      cmd (module Backend.Contents.Find);
      cmd (module Backend.Contents.Add);
      cmd (module Backend.Contents.Unsafe_add);
      cmd (module Backend.Contents.Index);
      cmd (module Backend.Contents.Clear);
      cmd (module Backend.Contents.Merge);
      cmd (module Backend.Node.Mem);
      cmd (module Backend.Node.Find);
      cmd (module Backend.Node.Add);
      cmd (module Backend.Node.Unsafe_add);
      cmd (module Backend.Node.Index);
      cmd (module Backend.Node.Clear);
      cmd (module Backend.Node.Merge);
      cmd (module Backend.Commit.Mem);
      cmd (module Backend.Commit.Find);
      cmd (module Backend.Commit.Add);
      cmd (module Backend.Commit.Unsafe_add);
      cmd (module Backend.Commit.Index);
      cmd (module Backend.Commit.Clear);
      cmd (module Backend.Commit.Merge);
      cmd (module Backend.Branch.Mem);
      cmd (module Backend.Branch.Find);
      cmd (module Backend.Branch.Set);
      cmd (module Backend.Branch.Test_and_set);
      cmd (module Backend.Branch.Remove);
      cmd (module Backend.Branch.List);
      cmd (module Backend.Branch.Clear);
      cmd (module Backend.Branch.Watch);
      cmd (module Backend.Branch.Unwatch);
      cmd (module Backend.Branch.Watch_key);
      cmd (module Stats);
      cmd (module Ping);
      cmd (module Set_current_branch);
      cmd (module Get_current_branch);
      cmd (module Import);
      cmd (module Export);
      cmd (module Branch_head);
      cmd (module Branch_set_head);
      cmd (module Branch_remove);
      cmd (module Commit_v);
      cmd (module Commit_of_key);
      cmd (module Contents_of_hash);
      cmd (module Contents_save);
      cmd (module Contents_exists);
      cmd (module Watch);
      cmd (module Unwatch);
    ]
    @ Store.commands @ Tree.commands

  let of_name name = List.assoc name commands
  let name (module Cmd : CMD) = Cmd.name
end
