open Lwt.Syntax
module Rpc =
  Irmin_server.Make (Irmin.Hash.BLAKE2B) (Irmin.Contents.String)
    (Irmin.Branch.String)

let main ~root ~addr ~port ~level =
  let open Rpc in
  let () = Logs.set_level (Logs.level_of_string level |> Result.get_ok) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let config = Irmin_pack.config root in
  let* ctx = Conduit_lwt_unix.init ~src:addr () in
  let* server = Server.v ~ctx ~port config in
  Logs.app (fun l -> l "Listening on: %s:%d" addr port);
  Server.serve server

let main root addr port level = Lwt_main.run @@ main ~root ~addr ~port ~level

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

let level =
  let doc = Arg.info ~doc:"Log level" [ "log-level" ] in
  Arg.(value @@ opt string "app" doc)

let main_term = Term.(const main $ root $ addr $ port $ level)

let () =
  let info = Term.info "irmin-server" in
  Term.exit @@ Term.eval (main_term, info)
