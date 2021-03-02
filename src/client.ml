open Lwt.Syntax
open Lwt.Infix
include Client_intf

module Make (C : Command.S) = struct
  module St = C.Store
  open C

  type conf = { host : Ipaddr.t; port : int }

  type t = { conf : conf; mutable conn : Conn.t }

  type hash = Store.hash

  type contents = Store.contents

  type branch = Store.branch

  type key = Store.key

  let conf ?(host = "127.0.0.1") ~port () =
    let host =
      Ipaddr.of_domain_name (Domain_name.of_string_exn host)
      |> Option.value ~default:(Ipaddr.of_string_exn host)
    in
    { host; port }

  let connect ?(ctx = Conduit_lwt_unix.default_ctx) conf =
    let c = `TCP (`IP conf.host, `Port conf.port) in
    let* flow, ic, oc = Conduit_lwt_unix.connect ~ctx c in
    let conn = Conn.v flow ic oc in
    let+ () = Handshake.V1.send oc in
    { conf; conn }

  let handle_disconnect t f =
    Lwt.catch
      (fun () ->
        let* x = f () in
        let+ () = Lwt_io.flush t.conn.oc in
        x)
      (function
        | End_of_file ->
            let* conn = connect t.conf in
            t.conn <- conn.conn;
            f ()
        | exn -> raise exn)

  let send_command_header t (module Cmd : C.CMD) =
    let n_args = fst Cmd.args in
    let header = Request.Header.v ~command:Cmd.name ~n_args in
    Request.Write.header t.conn.Conn.oc header

  let request t (type x y)
      (module Cmd : C.CMD with type res = x and type req = y) (a : y) =
    let name = Cmd.name in
    Logs.debug (fun l -> l "Starting request: command=%s" name);
    handle_disconnect t (fun () ->
        let* () = send_command_header t (module Cmd) in
        let args = Args.v ~count:(fst Cmd.args) t.conn in
        let* () = Cmd.Client.send args a in
        let* () = Lwt_io.flush t.conn.oc in
        let* res = Response.Read.header t.conn.ic in
        Response.Read.get_error t.conn.ic res >>= function
        | Some err ->
            Logs.debug (fun l ->
                l "Request error: command=%s, error=%s" name err);
            Lwt.return_error (`Msg err)
        | None ->
            let args = Args.v ~count:res.n_items t.conn in
            let+ x = Cmd.Client.recv args in
            assert (Args.remaining args = 0);
            Logs.debug (fun l -> l "Completed request: command=%s" name);
            x)

  let ping t = request t (module Commands.Ping) ()

  let set_branch t (branch : Store.branch) =
    request t (module Commands.Set_branch) branch

  module Store = struct
    let find t key = request t (module Commands.Store.Find) key

    let set t ~info key value =
      request t (module Commands.Store.Set) (key, info (), value)

    let remove t ~info key =
      request t (module Commands.Store.Remove) (key, info ())

    let find_tree t key = request t (module Commands.Store.Find_tree) key

    let set_tree t ~info key tree =
      request t (module Commands.Store.Set_tree) (key, info (), tree)
  end

  module Tree = struct
    type store = t

    include C.Tree

    let empty t = request t (module Commands.Tree.Empty) ()

    let add t tree key value =
      request t (module Commands.Tree.Add) (tree, key, value)

    let remove t tree key = request t (module Commands.Tree.Remove) (tree, key)

    let abort t tree = request t (module Commands.Tree.Abort) tree
  end
end
