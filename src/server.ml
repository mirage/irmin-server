open Lwt.Syntax
open Lwt.Infix
include Server_intf
module C = Command

module Make (X : Command.S) = struct
  module Command = X
  module Store = X.Store

  type t = {
    ctx : Conduit_lwt_unix.ctx;
    uri : Uri.t;
    server : Conduit_lwt_unix.server;
    config : Irmin.config;
    repo : Store.Repo.t;
  }

  let v ?tls_config ~uri conf =
    let uri = Uri.of_string uri in
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
    let* ctx = Conduit_lwt_unix.init ~src:addr () in
    let server =
      match String.lowercase_ascii scheme with
      | "unix" ->
          let file = Uri.path uri in
          at_exit (fun () -> try Unix.unlink file with _ -> ());
          `Unix_domain_socket (`File file)
      | "tcp" -> (
          let port = Uri.port uri |> Option.value ~default:8888 in
          match tls_config with
          | None -> `TCP (`Port port)
          | Some (`Cert_file crt, `Key_file key) ->
              `TLS
                ( `Crt_file_path crt,
                  `Key_file_path key,
                  `No_password,
                  `Port port ))
      | x -> invalid_arg ("Unknown server scheme: " ^ x)
    in
    let config = Irmin_pack_layered.config ~conf ~with_lower:true () in
    let+ repo = Store.Repo.v config in
    { ctx; uri; server; config; repo }

  let commands = Hashtbl.create 8

  let () = Hashtbl.replace_seq commands (List.to_seq Command.commands)

  let[@tailrec] rec loop repo conn client : unit Lwt.t =
    if Lwt_io.is_closed conn.Conn.ic then Lwt.return_unit
    else
      Lwt.catch
        (fun () ->
          let* Request.Header.{ command; n_args } =
            Request.Read.header conn.Conn.ic
          in
          match Hashtbl.find_opt commands command with
          | None ->
              let* () = Conn.err conn "unknown command" in
              let* () = Lwt_unix.yield () in
              loop repo conn client
          | Some (module Cmd : X.CMD) ->
              if n_args < fst Cmd.args then
                let* () = Conn.consume conn n_args in
                let* () =
                  Conn.err conn
                    (Format.sprintf "expected %d arguments but got %d"
                       (fst Cmd.args) n_args)
                in
                loop repo conn client
              else
                let args = Args.v ~mode:`Read ~count:n_args conn in
                let* return =
                  Lwt.catch
                    (fun () ->
                      let* args =
                        Cmd.Server.recv client args
                        >|= Error.unwrap "Invalid arguments"
                      in
                      Cmd.Server.handle conn client args)
                    (function
                      | Error.Error (a, b) -> raise (Error.Error (a, b))
                      | End_of_file -> raise End_of_file
                      | exn ->
                          raise
                            (Error.Error
                               (Args.remaining args - 1, Printexc.to_string exn)))
                in
                let () = Return.check return (snd Cmd.args) in
                Lwt.return_unit)
        (function
          | Error.Error (remaining, s) ->
              let* () = Conn.consume conn remaining in
              let* () = Conn.err conn s in
              Lwt_unix.sleep 0.01
          | End_of_file ->
              let* () = Lwt_io.close conn.ic in
              Lwt.return_unit
          | exn ->
              let* () = Lwt_io.close conn.ic in
              Logs.err (fun l -> l "Exception: %s" (Printexc.to_string exn));
              Lwt.return_unit)
      >>= fun () ->
      let* () = Lwt_io.flush conn.Conn.oc in
      let* () = Lwt_unix.yield () in
      loop repo conn client

  let callback repo flow ic oc =
    let conn = Conn.v flow ic oc in
    let* store = Store.master repo in
    let trees = Hashtbl.create 8 in
    let client = Command.{ conn; repo; store; trees } in
    let* check = Handshake.V1.check ic in
    if not check then
      let* () = Conn.err conn "invalid handshake" in
      Lwt_io.close ic
    else loop repo conn client

  let on_exn x = raise x

  let http_server (type x)
      (module Store : Irmin_pack_layered.S with type repo = x) (_repo : x) _conn
      _req body =
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string ~body:"OK" ~status:`OK ()

  let serve ?http { ctx; server; repo; _ } =
    let () =
      match http with
      | Some port ->
          let http =
            Cohttp_lwt_unix.Server.make
              ~callback:(http_server (module Store) repo)
              ()
          in
          Lwt.async (fun () ->
              Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) http)
      | None -> ()
    in
    Conduit_lwt_unix.serve ~ctx ~on_exn ~mode:server (callback repo)
end
