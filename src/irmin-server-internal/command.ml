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

  module Commands = struct
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

    module Backend = Command_backend.Make (IO) (Codec) (St) (Tree) (Commit)
    module Store = Command_store.Make (IO) (Codec) (St) (Tree) (Commit)
    module Tree = Command_tree.Make (IO) (Codec) (St) (Tree) (Commit)
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [
      cmd (module Ping);
      cmd (module Set_current_branch);
      cmd (module Get_current_branch);
      cmd (module Import);
      cmd (module Export);
    ]
    @ Store.commands @ Tree.commands @ Backend.commands

  let () = List.iter (fun (k, _) -> assert (String.length k < 255)) commands
  let of_name name = List.assoc name commands
  let name (module Cmd : CMD) = Cmd.name
end
