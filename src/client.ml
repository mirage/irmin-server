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

  let send_command_header t command =
    let n_args = n_args command in
    let header = Request.Header.v ~command ~n_args in
    Request.Write.header t.conn.Conn.oc header

  let request t command f g =
    let name = Command.name command in
    Logs.debug (fun l -> l "Starting request: command=%s" name);
    handle_disconnect t (fun () ->
        let* () = send_command_header t command in
        let* () = f t in
        let* () = Lwt_io.flush t.conn.oc in
        let* res = Response.Read.header t.conn.ic in
        Response.Read.get_error t.conn.ic res >>= function
        | Some err ->
            Logs.debug (fun l ->
                l "Request error: command=%s, error=%s" name err);
            Lwt.return_error (`Msg err)
        | None ->
            let args = Args.v ~count:res.n_items t.conn in
            let+ x = g args in
            assert (Args.remaining args = 0);
            Logs.debug (fun l -> l "Completed request: command=%s" name);
            x)

  let arg t ty x = Conn.write_arg t.conn ty x

  let ping t =
    request t Ping (fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_ok ())

  let set_branch t branch =
    request t SetBranch
      (fun t -> arg t Store.Branch.t branch)
      (fun _ -> Lwt.return_ok ())

  module Store = struct
    let find t key =
      request t Find
        (fun t -> arg t Store.Key.t key)
        (fun args -> Args.next args (Irmin.Type.option Store.contents_t))

    let set t ~info key value =
      request t Set
        (fun t ->
          let* () = arg t Store.Key.t key in
          let* () = arg t Irmin.Info.t (info ()) in
          arg t Store.Contents.t value)
        (fun _ -> Lwt.return_ok ())

    let remove t ~info key =
      request t Set
        (fun t ->
          let* () = arg t Store.Key.t key in
          arg t Irmin.Info.t (info ()))
        (fun _ -> Lwt.return_ok ())

    let find_tree t key =
      request t FindTree
        (fun t -> arg t Store.Key.t key)
        (fun args -> Args.next args (Irmin.Type.option Tree.t))

    let set_tree t ~info key tree =
      request t SetTree
        (fun t ->
          let* () = arg t Store.Key.t key in
          let* () = arg t Irmin.Info.t (info ()) in
          arg t Tree.t tree)
        (fun _ -> Lwt.return_ok ())
  end

  module Tree = struct
    type store = t

    include C.Tree

    let empty t =
      request t EmptyTree
        (fun _ -> Lwt.return_unit)
        (fun t -> Args.next t Tree.t)

    let add t tree key value =
      request t TreeAdd
        (fun t ->
          let* () = arg t Tree.t tree in
          let* () = arg t St.Key.t key in
          arg t St.contents_t value)
        (fun t -> Args.next t Tree.t)

    let remove t tree key =
      request t TreeRemove
        (fun t ->
          let* () = arg t Tree.t tree in
          arg t St.Key.t key)
        (fun t -> Args.next t Tree.t)
  end
end
