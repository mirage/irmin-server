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

    module Set_branch = struct
      type req = St.Branch.t

      type res = unit

      let args = (1, 0)

      let name = "set_branch"

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

    module Get_branch = struct
      type req = unit

      type res = St.Branch.t

      let args = (0, 1)

      let name = "get_branch"

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

    module Store = Command_store.Make (St)
    module Tree = Command_tree.Make (St)
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [
      cmd (module Ping);
      cmd (module Set_branch);
      cmd (module Get_branch);
      cmd (module Import);
      cmd (module Export);
    ]
    @ Store.commands @ Tree.commands

  let of_name name = List.assoc name commands

  let name (module Cmd : CMD) = Cmd.name

  let n_args (module C : CMD) = fst C.args

  let n_results (module C : CMD) = snd C.args
end
