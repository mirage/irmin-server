open Lwt.Syntax
open Lwt.Infix
include Command_intf

module Make (St : Irmin_pack_layered.S with type key = string list) = struct
  module Store = St
  include Context.Make (St)

  type t = (module CMD)

  module Commands = struct
    module Ping = struct
      let name = "ping"

      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let run conn _ctx () = Return.ok conn
    end

    module Set_current_branch = struct
      module Req = struct
        type t = St.Branch.t [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "set_current_branch"

      let run conn ctx branch =
        let* store = Store.of_branch ctx.repo branch in
        ctx.branch <- branch;
        ctx.store <- store;
        Return.ok conn
    end

    module Remove_branch = struct
      module Req = struct
        type t = St.Branch.t [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "remove_branch"

      let run conn ctx branch =
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

    module Get_current_branch = struct
      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = St.Branch.t [@@deriving irmin]
      end

      let name = "get_current_branch"

      let run conn ctx () = Return.v conn Store.Branch.t ctx.branch
    end

    module Export = struct
      module Req = struct
        type t = unit [@@deriving irmin]
      end

      module Res = struct
        type t = St.slice [@@deriving irmin]
      end

      let name = "export"

      let run conn ctx () =
        let* slice = Store.Repo.export ~full:true ctx.repo in
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

      let run conn ctx slice =
        let* () = Store.Repo.import ctx.repo slice >|= Error.unwrap "import" in
        Return.ok conn
    end

    module Head = struct
      module Req = struct
        type t = St.branch option [@@deriving irmin]
      end

      module Res = struct
        type t = Commit.t option [@@deriving irmin]
      end

      let name = "head"

      let run conn ctx branch =
        let branch = Option.value ~default:ctx.branch branch in
        let* head = Store.Branch.find ctx.repo branch in
        match head with
        | None -> Return.v conn (Irmin.Type.option Commit.t) None
        | Some head ->
            let info = Store.Commit.info head in
            let parents = Store.Commit.parents head in
            let hash = Store.Commit.hash head in
            let head = Commit.v ~info ~parents ~node:hash in
            Return.v conn (Irmin.Type.option Commit.t) (Some head)
    end

    module New_commit = struct
      module Req = struct
        type t = Irmin.Info.t * St.Hash.t list * Tree.t [@@deriving irmin]
      end

      module Res = struct
        type t = Commit.t [@@deriving irmin]
      end

      let name = "new_commit"

      let run conn ctx (info, parents, tree) =
        let* _, tree = resolve_tree ctx tree in
        let* commit = St.Commit.v ctx.repo ~info ~parents tree in
        let hash = St.Commit.hash commit in
        let head = Commit.v ~info ~parents ~node:hash in
        Return.v conn Commit.t head
    end

    module Set_head = struct
      module Req = struct
        type t = St.branch option * Commit.t [@@deriving irmin]
      end

      module Res = struct
        type t = unit [@@deriving irmin]
      end

      let name = "set_head"

      let run conn ctx (branch, commit) =
        let branch = Option.value ~default:ctx.branch branch in
        let* commit =
          St.Commit.of_hash ctx.repo (Commit.node commit) >|= Option.get
        in
        let* () = St.Branch.set ctx.repo branch commit in
        Return.ok conn
    end

    module Store = Command_store.Make (St)
    module Tree = Command_tree.Make (St)
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [
      cmd (module Ping);
      cmd (module Set_current_branch);
      cmd (module Get_current_branch);
      cmd (module Import);
      cmd (module Export);
      cmd (module Head);
      cmd (module New_commit);
      cmd (module Set_head);
      cmd (module Remove_branch);
    ]
    @ Store.commands @ Tree.commands

  let of_name name = List.assoc name commands

  let name (module Cmd : CMD) = Cmd.name
end
