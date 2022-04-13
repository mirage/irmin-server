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
      let cache_stats = Irmin_pack.Stats.get_cache_stats () in
      Lwt.return
        Stats.
          {
            uptime;
            branches;
            finds = pack.finds;
            cache_misses = cache_stats.cache_misses;
            adds = pack.appended_hashes + pack.appended_offsets;
          }

    let to_json = Irmin.Type.to_json_string t
  end

  module Commands = struct
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
      type res = St.contents_key [@@deriving irmin]

      let name = "contents.save"

      let run conn ctx _ contents =
        let* k =
          St.Backend.Repo.batch ctx.repo (fun t _ _ ->
              St.save_contents t contents)
        in
        Return.v conn res_t k
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
              Lwt.catch
                (fun () ->
                  let* () = Conn.write conn (Irmin.Diff.t Commit.t) diff in
                  IO.flush conn.oc)
                (fun _ -> Lwt.return_unit))
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

    module Backend = Command_backend.Make (IO) (Codec) (St) (Tree) (Commit)
    module Store = Command_store.Make (IO) (Codec) (St) (Tree) (Commit)
    module Tree = Command_tree.Make (IO) (Codec) (St) (Tree) (Commit)
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [
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
    @ Store.commands @ Tree.commands @ Backend.commands

  let () = List.iter (fun (k, _) -> assert (String.length k < 255)) commands
  let of_name name = List.assoc name commands
  let name (module Cmd : CMD) = Cmd.name
end
