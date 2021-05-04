open Lwt.Syntax
open Irmin_server_types

let main ~root ~uri ~tls ~store ~contents ~hash =
  let store, config =
    Irmin_unix.Resolver.load_config ~store ~hash ~contents ()
  in
  let (module Store : Irmin.S), _ = Irmin_unix.Resolver.Store.destruct store in
  let module Server = Irmin_server.Make (Store) in
  let tls_config =
    match tls with Some (c, k) -> Some (`Cert_file c, `Key_file k) | _ -> None
  in
  let* server = Server.v ?tls_config ~uri config in
  Logs.app (fun l -> l "Listening on %s, store: %s" uri root);
  Server.serve server

let main root uri tls (store, hash, contents) () =
  Lwt_main.run @@ main ~root ~uri ~tls ~store ~contents ~hash

open Cmdliner

let root =
  let doc = Arg.info ~doc:"Irmin store path" [ "r"; "root" ] in
  Arg.(value @@ opt string "/tmp/irmin-server" doc)

let tls =
  let doc = Arg.info ~docv:"CERT_FILE,KEY_FILE" ~doc:"TLS config" [ "tls" ] in
  Arg.(value @@ opt (some (pair string string)) None doc)

let main_term =
  Term.(const main $ root $ Cli.uri $ tls $ Cli.store $ Cli.setup_log)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
