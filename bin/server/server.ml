open Lwt.Syntax
open Irmin_server
module Rpc =
  Make (Irmin.Hash.BLAKE2B) (Irmin.Contents.String) (Irmin.Branch.String)

let main ~root ~uri ~tls ~level ~http =
  let open Rpc in
  let () = Logs.set_level (Some level) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let config = Irmin_pack.config root in
  let tls_config =
    match tls with Some (c, k) -> Some (`Cert_file c, `Key_file k) | _ -> None
  in
  let* server = Server.v ?tls_config ~uri config in
  Logs.app (fun l -> l "Listening on %s" uri);
  Server.serve ?http server

let main root uri tls level http =
  Lwt_main.run @@ main ~root ~uri ~tls ~level ~http

open Cmdliner

let root =
  let doc = Arg.info ~doc:"Irmin store path" [ "r"; "root" ] in
  Arg.(value @@ opt string "/tmp/irmin-server" doc)

let tls =
  let doc = Arg.info ~docv:"CERT_FILE,KEY_FILE" ~doc:"TLS config" [ "tls" ] in
  Arg.(value @@ opt (some (pair string string)) None doc)

let http =
  let doc =
    Arg.info ~doc:"Run the HTTP server on the specified port" ~docv:"PORT"
      [ "http" ]
  in
  Arg.(value @@ opt (some int) None doc)

let main_term = Term.(const main $ root $ Cli.uri $ tls $ Cli.log_level $ http)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
