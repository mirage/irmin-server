open Lwt.Syntax
open Irmin_server

let main ~root ~uri ~tls ~level ~http ~contents ~hash =
  let (module Contents : Irmin.Contents.S) =
    Irmin_unix.Resolver.Contents.find
      (Option.value contents ~default:Cli.default_contents)
  in
  let (module Hash : Irmin.Hash.S) =
    Option.value ~default:Cli.default_hash hash
  in
  let module Rpc = Make (Hash) (Contents) (Irmin.Branch.String) in
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

let main root uri tls level http contents hash =
  Lwt_main.run @@ main ~root ~uri ~tls ~level ~http ~contents ~hash

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

let main_term =
  Term.(
    const main $ root $ Cli.uri $ tls $ Cli.log_level $ http $ Cli.contents
    $ Cli.hash)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
