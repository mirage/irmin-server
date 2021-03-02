open Lwt.Syntax
module Rpc =
  Irmin_server.Make (Irmin.Hash.BLAKE2B) (Irmin.Contents.String)
    (Irmin.Branch.String)

let main ~root ~addr ~port ~ssl ~unix_socket ~level ~http =
  let open Rpc in
  let () = Logs.set_level (Logs.level_of_string level |> Result.get_ok) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let config = Irmin_pack.config root in
  let* ctx = Conduit_lwt_unix.init ~src:addr () in
  let tls_config =
    match ssl with Some (c, k) -> Some (`Cert_file c, `Key_file k) | _ -> None
  in
  let* server = Server.v ~ctx ~port ?unix_socket ?tls_config config in
  (match unix_socket with
  | Some s -> Logs.app (fun l -> l "Listening on unix://%s" s)
  | None -> Logs.app (fun l -> l "Listening on: %s:%d" addr port));
  Server.serve ~http server

let main root addr port ssl unix_socket level http =
  Lwt_main.run @@ main ~root ~addr ~port ~ssl ~unix_socket ~level ~http

open Cmdliner

let port =
  let doc = Arg.info ~doc:"Port to listen on" [ "p"; "port" ] in
  Arg.(value @@ opt int 8888 doc)

let root =
  let doc = Arg.info ~doc:"Irmin store path" [ "r"; "root" ] in
  Arg.(value @@ opt string "/tmp/irmin-server" doc)

let addr =
  let doc = Arg.info ~doc:"Address to listen on" [ "addr"; "a" ] in
  Arg.(value @@ opt string "127.0.0.1" doc)

let unix_socket =
  let doc = Arg.info ~doc:"Unix domain socket to listen on" [ "unix"; "u" ] in
  Arg.(value @@ opt (some string) None doc)

let ssl =
  let doc = Arg.info ~docv:"cert_file,key_file" ~doc:"SSL config" [ "ssl" ] in
  Arg.(value @@ opt (some (pair string string)) None doc)

let key_file =
  let doc = Arg.info ~doc:"TLS key file" [ "key-file" ] in
  Arg.(value @@ opt (some string) None doc)

let level =
  let doc = Arg.info ~doc:"Log level" [ "log-level" ] in
  Arg.(value @@ opt string "app" doc)

let http =
  let doc = Arg.info ~doc:"Run the HTTP server" [ "http" ] in
  Arg.(value @@ flag doc)

let main_term =
  Term.(const main $ root $ addr $ port $ ssl $ unix_socket $ level $ http)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
