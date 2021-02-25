open Lwt.Syntax
open Lwt.Infix
open Irmin_server_intf

module Conf = struct
  let entries = 32

  let stable_hash = 256
end

module Command = Command
module Args = Args
module Error = Error

module Make (H : Irmin.Hash.S) (C : Irmin.Contents.S) = struct
  module Store =
    Irmin_pack.Make (Conf) (Irmin.Metadata.None) (C) (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (H)

  type branch = string

  type hash = H.t

  type contents = C.t

  (*module Lazy_tree = struct
    type t = [ `Contents of Store.hash | `Tree of (Store.step * t) list ]
  end*)

  module Command = struct
    include Command

    type client = {
      conn : Conn.t;
      repo : Store.Repo.t;
      mutable store : Store.t;
    }

    let ping client _args = Conn.ok client.conn

    let set_branch client args =
      let* branch = Args.next args Store.Branch.t >|= Result.get_ok in
      let* store = Store.of_branch client.repo branch in
      client.store <- store;
      Conn.ok client.conn

    let cmd x n f = (x, (n, f))

    let commands = [ cmd Ping 0 ping; cmd SetBranch 1 set_branch ]

    let n_args cmd =
      let n, _ = List.assoc cmd commands in
      n
  end

  module Server = struct
    type t = {
      ctx : Conduit_lwt_unix.ctx;
      server : Conduit_lwt_unix.server;
      config : Irmin.config;
      repo : Store.Repo.t;
    }

    let v ?(ctx = Conduit_lwt_unix.default_ctx) ~port config =
      let server = `TCP (`Port port) in
      let+ repo = Store.Repo.v config in
      { ctx; server; config; repo }

    let commands = Hashtbl.create 8

    let () = Hashtbl.replace_seq commands (List.to_seq Command.commands)

    let callback repo flow ic oc =
      let conn = Conn.v flow ic oc in
      let* store = Store.master repo in
      let client = Command.{ conn; repo; store } in
      let* check = Handshake.V1.check ic in
      if not check then
        let* () = Conn.err conn "ERROR invalid handshake" in
        Lwt_io.close oc
      else
        let rec loop repo conn : unit Lwt.t =
          Lwt.catch
            (fun () ->
              let* Request.Header.{ command; n_args } =
                Request.Read.header ic
              in
              let req_args, cmd = Hashtbl.find commands command in
              if n_args < req_args then
                let* () = Conn.consume conn n_args in
                let* () =
                  Conn.err conn
                    (Format.sprintf "ERROR expected %d arguments but got %d"
                       req_args n_args)
                in
                loop repo conn
              else
                let* () = cmd client (Args.v ~count:n_args conn) in
                let* () = Lwt_unix.yield () in
                loop repo conn)
            (function
              | Error.Error (remaining, s) ->
                  let* () = Conn.consume conn remaining in
                  let* () = Conn.err conn s in
                  loop repo conn
              | End_of_file ->
                  let* () = Lwt_io.close oc in
                  Lwt.return_unit
              | exn ->
                  let* () = Lwt_io.close oc in
                  Lwt_io.printf "EXCEPTION: %s\n" (Printexc.to_string exn))
        in
        loop repo conn

    let on_exn x = raise x

    let serve { ctx; server; repo; _ } =
      Conduit_lwt_unix.serve ~ctx ~on_exn ~mode:server (callback repo)
  end

  module Client = struct
    type conf = { host : Ipaddr.t; port : int }

    type t = { conf : conf; mutable conn : Conn.t }

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
      handle_disconnect t (fun () ->
          let* () = send_command_header t command in
          let* () = f t in
          let* res = Response.Read.header t.conn.ic in
          let* error = Response.Read.get_error t.conn.ic res in
          match error with
          | Some err -> Lwt.return_error (`Msg err)
          | None -> g t)

    let ping t =
      request t Command.Ping
        (fun _ -> Lwt.return_unit)
        (fun _ -> Lwt.return_ok ())

    let set_branch t branch =
      request t Command.SetBranch
        (fun t -> Conn.write_arg t.conn Store.Branch.t branch)
        (fun _ -> Lwt.return_ok ())
  end
end
