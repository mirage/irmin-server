open Lwt.Syntax
open Lwt.Infix
include Server_intf

module Make (X : Command.S) = struct
  module Command = X
  module Store = Command.Store

  type t = {
    ctx : Conduit_lwt_unix.ctx;
    port : int;
    server : Conduit_lwt_unix.server;
    config : Irmin.config;
    repo : Store.Repo.t;
  }

  let v ?(ctx = Conduit_lwt_unix.default_ctx) ~port config =
    let server = `TCP (`Port port) in
    let+ repo = Store.Repo.v config in
    { ctx; server; config; repo; port }

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
          let req_args, res_count, cmd = Hashtbl.find commands command in
          if n_args < req_args then
            let* () = Conn.consume conn n_args in
            let* () =
              Conn.err conn
                (Format.sprintf "ERROR expected %d arguments but got %d"
                   req_args n_args)
            in
            loop repo conn client
          else
            let args = Args.v ~count:n_args conn in
            let* return =
              Lwt.catch
                (fun () -> cmd conn client args)
                (function
                  | Error.Error (a, b) -> raise (Error.Error (a, b))
                  | exn ->
                      raise
                        (Error.Error
                           (Args.remaining args - 1, Printexc.to_string exn)))
            in
            let () = Return.check return res_count in
            Lwt_io.flush conn.Conn.oc)
        (function
          | Error.Error (remaining, s) ->
              let* () = Conn.consume conn remaining in
              let* () = Conn.err conn s in
              let* () = Lwt_unix.sleep 0.1 in
              loop repo conn client
          | End_of_file ->
              let* () = Lwt_io.close conn.ic in
              Lwt.return_unit
          | exn ->
              let* () = Lwt_io.close conn.ic in
              Lwt_io.printf "EXCEPTION: %s\n" (Printexc.to_string exn))
      >>= fun () ->
      let* () = Lwt_unix.yield () in
      loop repo conn client

  let callback repo flow ic oc =
    let conn = Conn.v flow ic oc in
    let* store = Store.master repo in
    let client = Command.{ conn; repo; store } in
    let* check = Handshake.V1.check ic in
    if not check then
      let* () = Conn.err conn "ERROR invalid handshake" in
      Lwt_io.close ic
    else loop repo conn client

  let on_exn x = raise x

  let http_server (type x) (module Store : Irmin.S with type repo = x)
      (_repo : x) _conn _req body =
    Cohttp_lwt.Body.drain_body body >>= fun () ->
    Cohttp_lwt_unix.Server.respond_string ~body:"OK" ~status:`OK ()

  let serve ?(http = true) { ctx; server; repo; port; _ } =
    let () =
      if http then
        let http =
          Cohttp_lwt_unix.Server.make
            ~callback:(http_server (module Store) repo)
            ()
        in
        Lwt.async (fun () ->
            Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port (port + 1))) http)
    in
    Conduit_lwt_unix.serve ~ctx ~on_exn ~mode:server (callback repo)
end
