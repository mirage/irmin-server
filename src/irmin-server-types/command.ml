open Lwt.Syntax
open Lwt.Infix
include Command_intf

module Make (St : STORE) = struct
  module Store = St
  module Tree = Tree.Make (St)
  module Commit = Commit.Make (St) (Tree)
  include Context.Make (St) (Tree)

  type t = (module CMD)

  let convert_commit head =
    let info = Store.Commit.info head in
    let parents = Store.Commit.parents head in
    let hash = Store.Commit.hash head in
    let tree = Tree.Hash (Store.Commit.tree head |> Store.Tree.hash) in
    Commit.v ~info ~parents ~hash ~tree

  module Stats = struct
    type t = Stats.t

    let t = Stats.t

    let v repo info : t Lwt.t =
      let pack = Irmin_pack.Stats.get () in
      let uptime = Server_info.uptime info in
      let* branches =
        St.Branch.list repo >|= List.map (Irmin.Type.to_string St.Branch.t)
      in
      Lwt.return
        Stats.
          {
            uptime;
            branches;
            finds = pack.finds;
            cache_misses = pack.cache_misses;
            adds = pack.appended_hashes + pack.appended_offsets;
          }

    let to_json = Irmin.Type.to_json_string t
  end

  module Commands = struct
    module Stats = struct
      let name = "stats"

      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = Stats.t [@@deriving irmin]
      end

      let run conn ctx info () =
        let* stats = Stats.v ctx.repo info in
        Return.v conn Res.t stats
    end

    module Ping = struct
      let name = "ping"

      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let run conn _ctx _ () = Return.ok conn
    end

    module Set_current_branch = struct
      module Req = struct
        type t = St.Branch.t [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "set_current_branch"

      let run conn ctx _ branch =
        let* store = Store.of_branch ctx.repo branch in
        ctx.branch <- branch;
        ctx.store <- store;
        Return.ok conn
    end

    module Get_current_branch = struct
      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = St.Branch.t [@@deriving irmin]
      end

      let name = "get_current_branch"

      let run conn ctx _ () = Return.v conn Store.Branch.t ctx.branch
    end

    module Export = struct
      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = St.slice [@@deriving irmin]
      end

      let name = "export"

      let run conn ctx _ () =
        let* slice = Store.Repo.export ~full:true ~max:`Head ctx.repo in
        Return.v conn Store.slice_t slice
    end

    module Import = struct
      module Req = struct
        type t = St.slice [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "import"

      let run conn ctx _ slice =
        let* () = Store.Repo.import ctx.repo slice >|= Error.unwrap "import" in
        Return.ok conn
    end

    module Branch_remove = struct
      module Req = struct
        type t = St.Branch.t [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "branch.remove"

      let run conn ctx _ branch =
        let* () = Store.Branch.remove ctx.repo branch in
        let* () =
          if Irmin.Type.(unstage (equal Store.Branch.t)) ctx.branch branch then
            let+ store = Store.master ctx.repo in
            let () = ctx.branch <- Store.Branch.master in
            ctx.store <- store
          else Lwt.return_unit
        in
        Return.ok conn
    end

    module Branch_head = struct
      module Req = struct
        type t = St.branch option [@@deriving irmin]
      end

      module Res = struct
        type t = Commit.t option [@@deriving irmin]
      end

      let name = "branch.head"

      let run conn ctx _ branch =
        let branch = Option.value ~default:ctx.branch branch in
        let* head = Store.Branch.find ctx.repo branch in
        match head with
        | None -> Return.v conn Res.t None
        | Some head -> Return.v conn Res.t (Some (convert_commit head))
    end

    module Branch_set_head = struct
      module Req = struct
        type t = St.branch option * Commit.t [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "branch.set_head"

      let run conn ctx _ (branch, commit) =
        let branch = Option.value ~default:ctx.branch branch in
        let* commit =
          St.Commit.of_hash ctx.repo (Commit.hash commit) >|= Option.get
        in
        let* () = St.Branch.set ctx.repo branch commit in
        Return.ok conn
    end

    module Commit_v = struct
      module Req = struct
        type t = Irmin.Info.t * St.Hash.t list * Tree.t [@@deriving irmin]
      end

      module Res = struct
        type t = Commit.t [@@deriving irmin]
      end

      let name = "commit.v"

      let run conn ctx _ (info, parents, tree) =
        let* _, tree = resolve_tree ctx tree in
        let* commit = St.Commit.v ctx.repo ~info ~parents tree in
        let hash = St.Commit.hash commit in
        let tree_ = St.Commit.tree commit in
        let tree = Tree.Hash (St.Commit.tree commit |> St.Tree.hash) in
        let head = Commit.v ~info ~parents ~hash ~tree in
        St.Tree.clear tree_;
        reset_trees ctx;
        Return.v conn Commit.t head
    end

    module Flush = struct
      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "flush"

      let run conn ctx _ () =
        St.flush ctx.repo;
        Return.v conn Res.t ()
    end

    module Commit_of_hash = struct
      module Req = struct
        type t = St.Hash.t [@@deriving irmin]
      end

      module Res = struct
        type t = Commit.t option [@@deriving irmin]
      end

      let name = "commit.of_hash"

      let run conn ctx _ hash =
        let* commit = St.Commit.of_hash ctx.repo hash in
        let commit = Option.map convert_commit commit in
        Return.v conn Res.t commit
    end

    module Contents_of_hash = struct
      module Req = struct
        type t = St.Hash.t [@@deriving irmin]
      end

      module Res = struct
        type t = St.contents option [@@deriving irmin]
      end

      let name = "contents.of_hash"

      let run conn ctx _ hash =
        let* contents = St.Contents.of_hash ctx.repo hash in
        Return.v conn Res.t contents
    end

    module Contents_save = struct
      module Req = struct
        type t = St.contents [@@deriving irmin]
      end

      module Res = struct
        type t = St.Hash.t [@@deriving irmin]
      end

      let name = "contents.save"

      let run conn ctx _ contents =
        let* hash =
          St.Private.Repo.batch ctx.repo (fun t _ _ ->
              St.save_contents t contents)
        in
        Return.v conn Res.t hash
    end

    module Contents_exists = struct
      module Req = struct
        type t = St.Hash.t [@@deriving irmin]
      end

      module Res = struct
        type t = bool [@@deriving irmin]
      end

      let name = "contents.exists"

      let run conn ctx _ hash =
        let* exists =
          St.Private.Repo.batch ctx.repo (fun t _ _ ->
              St.Private.Contents.mem t hash)
        in
        Return.v conn Res.t exists
    end

    module Store = Command_store.Make (St) (Tree) (Commit)
    module Tree = Command_tree.Make (St) (Tree) (Commit)
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
      cmd (module Commit_of_hash);
      cmd (module Contents_of_hash);
      cmd (module Contents_save);
      cmd (module Contents_exists);
      cmd (module Flush);
    ]
    @ Store.commands @ Tree.commands

  let of_name name = List.assoc name commands

  let name (module Cmd : CMD) = Cmd.name
end
