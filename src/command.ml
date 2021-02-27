open Lwt.Syntax
open Lwt.Infix
include Command_intf

module Make (St : Irmin_pack_layered.S) = struct
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
        let handle conn _context _args = Return.ok conn
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
        let handle conn ctx args =
          let* branch = Args.next args St.Branch.t >|= Error.unwrap in
          let* store = Store.of_branch ctx.repo branch in
          ctx.store <- store;
          Return.ok conn
      end

      module Client = struct
        let send t branch : unit Lwt.t = Args.write t Store.Branch.t branch

        let recv _args : res Error.result Lwt.t = Lwt.return_ok ()
      end
    end

    module Store = Command_store.Make (St)
    module Tree = Command_tree.Make (St)
  end

  let commands : (string * (module CMD)) list =
    let open Commands in
    [ cmd (module Ping); cmd (module Set_branch) ]
    @ Store.commands @ Tree.commands

  let of_name name = List.assoc name commands

  let name (module Cmd : CMD) = Cmd.name

  (*module X = struct
      module XTree = struct
        let empty conn ctx _args =
          let empty = St.Tree.empty in
          let id = Random.bits () in
          Hashtbl.replace ctx.trees id empty;
          Return.v conn Tree.t (ID id)

        let add conn ctx args =
          let* tree = Args.next args Tree.t >|= Error.unwrap in
          let* key = Args.next args Store.Key.t >|= Error.unwrap in
          let* value = Args.next args Store.contents_t >|= Error.unwrap in
          let* id, tree = resolve ctx tree in
          let* tree = Store.Tree.add tree key value in
          Hashtbl.replace ctx.trees id tree;
          Return.v conn Tree.t (ID id)

        let remove conn ctx args =
          let* tree = Args.next args Tree.t >|= Error.unwrap in
          let* key = Args.next args Store.Key.t >|= Error.unwrap in
          let* id, tree = resolve ctx tree in
          let* tree = Store.Tree.remove tree key in
          Hashtbl.replace ctx.trees id tree;
          Return.v conn Tree.t (ID id)
      end

      module Tree = XTree
    end*)

  (*let cmd x n_in n_out f = (x, (n_in, n_out, f))

    let commands =
      let open X in
      [
        cmd FindTree 1 1 Store.find_tree;
        cmd SetTree 3 1 Store.set_tree;
        (* Tree *)
        cmd TreeAdd 3 1 Tree.add;
        cmd TreeRemove 2 1 Tree.remove;
        cmd EmptyTree 0 1 Tree.empty;
      ]*)

  let n_args (module C : CMD) = fst C.args

  let n_results (module C : CMD) = snd C.args
end
