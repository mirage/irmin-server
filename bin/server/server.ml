open Lwt.Syntax
module Rpc =
  Irmin_server.Make (Irmin.Hash.BLAKE2B) (Irmin.Contents.String)
    (Irmin.Branch.String)

let main ~root ~uri ~ssl ~level ~http =
  let open Rpc in
  let () = Logs.set_level (Logs.level_of_string level |> Result.get_ok) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let config = Irmin_pack.config root in
  let tls_config =
    match ssl with Some (c, k) -> Some (`Cert_file c, `Key_file k) | _ -> None
  in
  let* server = Server.v ?tls_config ~uri config in
  Logs.app (fun l -> l "Listening on %s" uri);
  Server.serve ?http server

let main root uri ssl level http =
  Lwt_main.run @@ main ~root ~uri ~ssl ~level ~http

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
  Arg.(value @@ opt (some int) None doc)

let uri =
  let doc = Arg.info ~docv:"URL" ~doc:"URI to connect to" [ "uri"; "u" ] in
  Arg.(value & opt string "tcp://127.0.0.1:8888" & doc)

let main_term = Term.(const main $ root $ uri $ ssl $ level $ http)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
