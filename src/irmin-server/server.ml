open Lwt.Syntax
open Lwt.Infix
open Irmin_server_internal
include Server_intf

module Make (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) = struct
  module Command = Command.Make (IO) (Codec) (Store)
  module Store = Store
  module Conn = Command.Conn

  type t = {
    ctx : Conduit_lwt_unix.ctx;
    uri : Uri.t;
    server : Conduit_lwt_unix.server;
    config : Irmin.config;
    repo : Store.Repo.t;
    clients : (Command.context, unit) Hashtbl.t;
    info : Command.Server_info.t;
  }

  module Client_set = Set.Make (struct
    type t = Command.context

    let compare a b =
      let conn = a.Command.conn.ic in
      let conn' = b.Command.conn.ic in
      compare conn conn'
  end)

  let readonly conf =
    Irmin.Backend.Conf.add conf Irmin_pack.Conf.Key.readonly true

  let v ?tls_config ~uri config =
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let* ctx, server =
      match String.lowercase_ascii scheme with
      | "unix" ->
          let file = Uri.path uri in
          let+ () =
            Lwt.catch
              (fun () -> Lwt_unix.unlink file)
              (fun _ -> Lwt.return_unit)
          in
          ( Lazy.force Conduit_lwt_unix.default_ctx,
            `Unix_domain_socket (`File file) )
      | "tcp" -> (
          let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
          let ip = Unix.gethostbyname addr in
          let addr = ip.h_addr_list.(0) |> Unix.string_of_inet_addr in
          let+ ctx = Conduit_lwt_unix.init ~src:addr () in
          let port = Uri.port uri |> Option.value ~default:9181 in
          match tls_config with
          | None -> (ctx, `TCP (`Port port))
          | Some (`Cert_file crt, `Key_file key) ->
              ( ctx,
                `TLS
                  ( `Crt_file_path crt,
                    `Key_file_path key,
                    `No_password,
                    `Port port ) ))
      | x -> invalid_arg ("Unknown server scheme: " ^ x)
    in
    let+ repo = Store.Repo.v config in
    let clients = Hashtbl.create 8 in
    let start_time = Unix.time () in
    let info = Command.Server_info.{ start_time } in
    { ctx; uri; server; config; repo; clients; info }

  let commands = Hashtbl.create (List.length Command.commands)
  let () = Hashtbl.replace_seq commands (List.to_seq Command.commands)
  let invalid_arguments a = Error.unwrap "Invalid arguments" a [@@inline]

  let[@tailrec] rec loop repo clients conn client info : unit Lwt.t =
    if Conn.is_closed conn then
      let* () =
        match client.Command.watch with
        | Some w -> Store.unwatch w
        | None -> Lwt.return_unit
      in
      let* () =
        match client.Command.branch_watch with
        | Some w ->
            let b = Store.Backend.Repo.branch_t client.repo in
            Store.Backend.Branch.unwatch b w
        | None -> Lwt.return_unit
      in
      let () = Hashtbl.remove clients client in
      Lwt.return_unit
    else
      Lwt.catch
        (fun () ->
          Logs.debug (fun l -> l "Receiving next command");
          (* Get request header (command and number of arguments) *)
          let* Conn.Request.{ command } = Conn.Request.read_header conn in
          (* Get command *)
          match Hashtbl.find_opt commands command with
          | None ->
              if String.length command = 0 then Lwt.return_unit
              else
                let () = Logs.err (fun l -> l "Unknown command: %s" command) in
                Conn.err conn ("unknown command: " ^ command)
          | Some (module Cmd : Command.CMD) ->
              let* req = Conn.read conn Cmd.req_t >|= invalid_arguments in
              Logs.debug (fun l -> l "Command: %s" Cmd.name);
              let* res = Cmd.run conn client info req in
              Conn.Return.finish res)
        (function
          | Error.Error s ->
              (* Recover *)
              let backtrace = Printexc.get_backtrace () in
              Logs.debug (fun l -> l "Error response: %s\n%s" s backtrace);
              let* () = Conn.err conn s in
              Lwt_unix.sleep 0.01
          | End_of_file ->
              (* Client has disconnected *)
              let* () = Lwt_io.close conn.ic in
              Lwt.return_unit
          | exn ->
              if Conn.is_closed conn then Lwt.return_unit
              else
                (* Unhandled exception *)
                let s = Printexc.to_string exn in
                Logs.err (fun l ->
                    l "Exception: %s\n%s" s (Printexc.get_backtrace ()));
                let* () = Conn.err conn s in
                Lwt_unix.sleep 0.01)
      >>= fun () -> loop repo clients conn client info

  let callback { repo; clients; info; config; _ } flow ic oc =
    (* Handshake check *)
    let conn = Conn.v flow ic oc in
    let* check =
      Lwt.catch
        (fun () -> Conn.Handshake.V1.check (module Store) conn)
        (fun _ -> Lwt.return_false)
    in
    if not check then
      (* Hanshake failed *)
      let () =
        Logs.info (fun l -> l "Client closed because of invalid handshake")
      in
      Lwt_io.close ic
    else
      (* Handshake ok *)
      let branch = Store.Branch.main in
      let* store = Store.of_branch repo branch in
      let trees = Hashtbl.create 8 in
      let client =
        Command.
          {
            conn;
            repo;
            branch;
            store;
            trees;
            watch = None;
            branch_watch = None;
            config;
          }
      in
      Hashtbl.replace clients client ();
      loop repo clients conn client info

  let on_exn x = Logs.err (fun l -> l "EXCEPTION: %s" (Printexc.to_string x))

  let serve ?stop t =
    let unlink () =
      match Uri.scheme t.uri with
      | Some "unix" -> Unix.unlink (Uri.path t.uri)
      | _ -> ()
    in
    let _ =
      Lwt_unix.on_signal Sys.sigint (fun _ ->
          unlink ();
          exit 0)
    in
    let _ =
      Lwt_unix.on_signal Sys.sigterm (fun _ ->
          unlink ();
          exit 0)
    in
    let* () =
      Conduit_lwt_unix.serve ?stop ~ctx:t.ctx ~on_exn ~mode:t.server
        (callback t)
    in
    Lwt.wrap (fun () -> unlink ())
end
