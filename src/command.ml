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

  type client = { conn : Conn.t; repo : Store.Repo.t; mutable store : Store.t }

  module Return = struct
    type t = { n_items : int; mutable index : int; client : client }

    let make n_items client =
      let x = { n_items; index = 0; client } in
      let+ () =
        Response.Write.header client.conn.oc Response.Header.{ n_items }
      in
      x

    let ok client = make 0 client

    let err client msg =
      let* t = make (-1) client in
      let+ () = Message.write client.conn.oc Irmin.Type.string msg in
      t

    let write ty x t =
      let+ () = Message.write t.client.conn.oc ty x in
      t.index <- t.index + 1;
      t

    let v client ty x =
      let* r = make 1 client in
      write ty x r

    let check t = assert (t.index = t.n_items || t.n_items = -1)
  end

  type f = client -> Args.t -> Return.t Lwt.t

  let ping client _args = Return.ok client

  let set_branch client args =
    let* branch = Args.next args Store.Branch.t >|= Result.get_ok in
    let* store = Store.of_branch client.repo branch in
    client.store <- store;
    Return.ok client

  let find client args =
    let* key = Args.next args Store.Key.t >|= Result.get_ok in
    let* x = Store.find client.store key in
    let* () = Conn.begin_response client.conn 1 in
    Return.v client (Irmin.Type.option Store.contents_t) x

  let set client args =
    let* key = Args.next args Store.Key.t >|= Result.get_ok in
    let* info = Args.next args Irmin.Info.t >|= Result.get_ok in
    let* value = Args.next args Store.Contents.t >|= Result.get_ok in
    let* () = Store.set_exn client.store key value ~info:(fun () -> info) in
    Return.ok client

  let remove client args =
    let* key = Args.next args Store.Key.t >|= Result.get_ok in
    let* info = Args.next args Irmin.Info.t >|= Result.get_ok in
    let* () = Store.remove_exn client.store key ~info:(fun () -> info) in
    Return.ok client

  let cmd x n f = (x, (n, f))

  let commands =
    [
      cmd Ping 0 ping;
      cmd SetBranch 1 set_branch;
      cmd Find 1 find;
      cmd Set 3 set;
      cmd Remove 2 remove;
    ]

  let n_args cmd =
    let n, _ = List.assoc cmd commands in
    n
end
