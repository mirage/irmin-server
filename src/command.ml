open Lwt.Syntax
open Lwt.Infix
include Command_intf

let of_name name =
  Irmin.Type.of_string command_t ("\"" ^ name ^ "\"") |> Result.get_ok

let name x =
  let s = Irmin.Type.to_string command_t x in
  if String.length s < 2 then invalid_arg "invalid command name"
  else String.sub s 1 (String.length s - 2)

module Make (Store : Irmin.S) = struct
  type t = command

  module Store = Store

  type context = { conn : Conn.t; repo : Store.Repo.t; mutable store : Store.t }

  type f = Conn.t -> context -> Args.t -> Return.t Lwt.t

  module X = struct
    let ping conn _ctx _args = Return.ok conn

    let set_branch conn ctx args =
      let* branch = Args.next args Store.Branch.t >|= Result.get_ok in
      let* store = Store.of_branch ctx.repo branch in
      ctx.store <- store;
      Return.ok conn

    module Store = struct
      let find conn ctx args =
        let* key = Args.next args Store.Key.t >|= Result.get_ok in
        let* x = Store.find ctx.store key in
        let* () = Conn.begin_response ctx.conn 1 in
        Return.v conn (Irmin.Type.option Store.contents_t) x

      let set conn ctx args =
        let* key = Args.next args Store.Key.t >|= Result.get_ok in
        let* info = Args.next args Irmin.Info.t >|= Result.get_ok in
        let* value = Args.next args Store.Contents.t >|= Result.get_ok in
        let* () = Store.set_exn ctx.store key value ~info:(fun () -> info) in
        Return.ok conn

      let remove conn ctx args =
        let* key = Args.next args Store.Key.t >|= Result.get_ok in
        let* info = Args.next args Irmin.Info.t >|= Result.get_ok in
        let* () = Store.remove_exn ctx.store key ~info:(fun () -> info) in
        Return.ok conn
    end
  end

  let cmd x n_in n_out f = (x, (n_in, n_out, f))

  let commands =
    let open X in
    [
      cmd Ping 0 0 ping;
      cmd SetBranch 1 0 set_branch;
      cmd Find 1 1 Store.find;
      cmd Set 3 0 Store.set;
      cmd Remove 2 0 Store.remove;
    ]

  let n_args cmd =
    let n, _, _ = List.assoc cmd commands in
    n

  let n_results cmd =
    let _, n, _ = List.assoc cmd commands in
    n
end
