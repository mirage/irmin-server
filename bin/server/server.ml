open Lwt.Syntax
open Irmin_server

let main ~root ~uri ~tls ~level ~contents ~hash =
  let (module Contents : Irmin.Contents.S) =
    Irmin_unix.Resolver.Contents.find
      (Option.value contents ~default:Cli.default_contents)
  in
  let (module Hash : Irmin.Hash.S) =
    Option.value ~default:Cli.default_hash hash
  in
  let module Rpc =
    Make
      (struct
        let version = `V1
      end)
      (Conf.Default)
      (Irmin.Metadata.None)
      (Contents)
      (Irmin.Branch.String)
      (Hash)
  in
  let open Rpc in
  let () = Logs.set_level (Some level) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let config = Irmin_pack.config root in
  let tls_config =
    match tls with Some (c, k) -> Some (`Cert_file c, `Key_file k) | _ -> None
  in
  let* server = Server.v ?tls_config ~uri config in
  Logs.app (fun l -> l "Listening on %s" uri);
  Server.serve server

let main root uri tls level contents hash =
  Lwt_main.run @@ main ~root ~uri ~tls ~level ~contents ~hash

open Cmdliner

let root =
  let doc = Arg.info ~doc:"Irmin store path" [ "r"; "root" ] in
  Arg.(value @@ opt string "/tmp/irmin-server" doc)

let tls =
  let doc = Arg.info ~docv:"CERT_FILE,KEY_FILE" ~doc:"TLS config" [ "tls" ] in
  Arg.(value @@ opt (some (pair string string)) None doc)

let main_term =
  Term.(
    const main $ root $ Cli.uri $ tls $ Cli.log_level $ Cli.contents $ Cli.hash)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
