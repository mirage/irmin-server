open Lwt.Syntax
open Lwt.Infix
include Client_intf
module C = Command

module Make (Command : Command.S) = struct
  module Store = Command.Store

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

  let send_command_header t command =
    let n_args = Command.n_args command in
    let header = Request.Header.v ~command ~n_args in
    Request.Write.header t.conn.Conn.oc header

  let request t command f g =
    Logs.debug (fun l -> l "Starting request: command=%s" (C.name command));
    handle_disconnect t (fun () ->
        let* () = send_command_header t command in
        let* () = f t in
        let* () = Lwt_io.flush t.conn.oc in
        let* res = Response.Read.header t.conn.ic in
        Response.Read.get_error t.conn.ic res >>= function
        | Some err ->
            let* () = Conn.consume t.conn res.n_items in
            Logs.debug (fun l ->
                l "Request error: command=%s, error=%s" (C.name command) err);
            Lwt.return_error (`Msg err)
        | None ->
            let args = Args.v ~count:res.n_items t.conn in
            let+ x = g args in
            assert (Args.remaining args = 0);
            Logs.debug (fun l ->
                l "Completed request: command=%s" (C.name command));
            x)

  let ping t =
    request t Ping (fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_ok ())

  let set_branch t branch =
    request t SetBranch
      (fun t -> Conn.write_arg t.conn Store.Branch.t branch)
      (fun _ -> Lwt.return_ok ())

  let find t key =
    request t Find
      (fun t -> Conn.write_arg t.conn Store.Key.t key)
      (fun args -> Args.next args (Irmin.Type.option Store.contents_t))

  let set t ~info key value =
    request t Set
      (fun t ->
        let* () = Conn.write_arg t.conn Store.Key.t key in
        let* () = Conn.write_arg t.conn Irmin.Info.t (info ()) in
        Conn.write_arg t.conn Store.Contents.t value)
      (fun _ -> Lwt.return_ok ())

  let remove t ~info key =
    request t Set
      (fun t ->
        let* () = Conn.write_arg t.conn Store.Key.t key in
        Conn.write_arg t.conn Irmin.Info.t (info ()))
      (fun _ -> Lwt.return_ok ())
end
