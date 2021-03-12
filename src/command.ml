open Lwt.Syntax
open Lwt.Infix
include Command_intf

module Make (St : Irmin_pack_layered.S with type key = string list) = struct
  module Store = St
  include Context.Make (St)

  type t = (module CMD)

  module Commands = struct
    module Ping = struct
      type req = unit

      type res = unit

      let args = (0, 0)

      let name = "ping"

      module Server = struct
        let recv _context _args = Lwt.return_ok ()

        let handle conn _context () = Return.ok conn
      end

      module Client = struct
        let send _args _req = Lwt.return_unit

        let recv _args = Lwt.return_ok ()
      end
    end

    module Set_current_branch = struct
      type req = St.Branch.t

      type res = unit

      let args = (1, 0)

      let name = "set_current_branch"

      module Server = struct
        let recv _ctx args = Args.next args St.Branch.t

        let handle conn ctx branch =
          let* store = Store.of_branch ctx.repo branch in
          ctx.branch <- branch;
          ctx.store <- store;
          Return.ok conn
      end

      module Client = struct
        let send t branch : unit Lwt.t = Args.write t Store.Branch.t branch

        let recv _args : res Error.result Lwt.t = Lwt.return_ok ()
      end
    end

    module Remove_branch = struct
      type req = St.Branch.t

      type res = unit

      let args = (1, 0)

      let name = "remove_branch"

      module Server = struct
        let recv _ctx args = Args.next args St.Branch.t

        let handle conn ctx branch =
          let* () = Store.Branch.remove ctx.repo branch in
          let* () =
            if Irmin.Type.(unstage (equal Store.Branch.t)) ctx.branch branch
            then
              let+ store = Store.of_branch ctx.repo branch in
              let () = ctx.branch <- Store.Branch.master in
              ctx.store <- store
            else Lwt.return_unit
          in
          Return.ok conn
      end

      module Client = struct
        let send t branch : unit Lwt.t = Args.write t Store.Branch.t branch

        let recv _args : res Error.result Lwt.t = Lwt.return_ok ()
      end
    end

    module Get_current_branch = struct
      type req = unit

      type res = St.Branch.t

      let args = (0, 1)

      let name = "get_current_branch"

      module Server = struct
        let recv _ctx _args = Lwt.return_ok ()

        let handle conn ctx () = Return.v conn Store.Branch.t ctx.branch
      end

      module Client = struct
        let send _t _branch : unit Lwt.t = Lwt.return_unit

        let recv args : res Error.result Lwt.t = Args.next args Store.Branch.t
      end
    end

    module Export = struct
      type req = unit

      type res = St.slice

      let args = (0, 1)

      let name = "export"

      module Server = struct
        let recv _ctx _args = Lwt.return_ok ()

        let handle conn ctx () =
          let* slice = Store.Repo.export ~full:true ctx.repo in
          Return.v conn Store.slice_t slice
      end

      module Client = struct
        let send _t () : unit Lwt.t = Lwt.return_unit

        let recv args : res Error.result Lwt.t = Args.next args Store.slice_t
      end
    end

    module Import = struct
      type req = St.slice

      type res = unit

      let args = (1, 0)

      let name = "import"

      module Server = struct
        let recv _ctx args = Args.next args Store.slice_t

        let handle conn ctx slice =
          let* () =
            Store.Repo.import ctx.repo slice >|= Error.unwrap "import"
          in
          Return.ok conn
      end

      module Client = struct
        let send t slice : unit Lwt.t = Args.write t Store.slice_t slice

        let recv _args : res Error.result Lwt.t = Lwt.return_ok ()
      end
    end

    module Head = struct
      type req = St.branch option

      type res = Commit.t option

      let args = (1, 1)

      let name = "head"

      module Server = struct
        let recv _ctx args = Args.next args (Irmin.Type.option St.branch_t)

        let handle conn ctx branch =
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

      module Client = struct
        let send t b : unit Lwt.t =
          Args.write t (Irmin.Type.option Store.branch_t) b

        let recv args : res Error.result Lwt.t =
          Args.next args (Irmin.Type.option Commit.t)
      end
    end

    module New_commit = struct
      type req = Irmin.Info.t * St.hash list * Tree.t

      type res = Commit.t

      let args = (3, 1)

      let name = "new_commit"

      module Server = struct
        let recv _ctx args =
          let* info = Args.next args Irmin.Info.t >|= Error.unwrap "info" in
          let* parents =
            Args.next args (Irmin.Type.list St.Hash.t)
            >|= Error.unwrap "parents"
          in
          let+ tree = Args.next args Tree.t >|= Error.unwrap "tree" in
          Ok (info, parents, tree)

        let handle conn ctx (info, parents, tree) =
          let* _, tree = resolve_tree ctx tree in
          let* commit = St.Commit.v ctx.repo ~info ~parents tree in
          let hash = St.Commit.hash commit in
          let head = Commit.v ~info ~parents ~node:hash in
          Return.v conn Commit.t head
      end

      module Client = struct
        let send t (info, parents, tree) : unit Lwt.t =
          let* () = Args.write t Irmin.Info.t info in
          let* () = Args.write t (Irmin.Type.list St.Hash.t) parents in
          Args.write t Tree.t tree

        let recv args : res Error.result Lwt.t = Args.next args Commit.t
      end
    end

    module Set_head = struct
      type req = St.branch option * Commit.t

      type res = unit

      let args = (2, 0)

      let name = "set_head"

      module Server = struct
        let recv _ctx args =
          let* branch =
            Args.next args (Irmin.Type.option St.branch_t)
            >|= Error.unwrap "branch"
          in
          let+ commit = Args.next args Commit.t >|= Error.unwrap "commit" in
          Ok (branch, commit)

        let handle conn ctx (branch, commit) =
          let branch = Option.value ~default:ctx.branch branch in
          let* commit =
            St.Commit.of_hash ctx.repo (Commit.node commit) >|= Option.get
          in
          let* () = St.Branch.set ctx.repo branch commit in
          Return.ok conn
      end

      module Client = struct
        let send t (b, c) : unit Lwt.t =
          let* () = Args.write t (Irmin.Type.option St.branch_t) b in
          Args.write t Commit.t c

        let recv _args : res Error.result Lwt.t = Lwt.return_ok ()
      end
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

  let n_args (module C : CMD) = fst C.args

  let n_results (module C : CMD) = snd C.args
end
