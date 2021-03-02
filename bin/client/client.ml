open Cmdliner
open Lwt.Syntax
open Lwt.Infix
open Irmin_server
module Rpc =
  Make (Irmin.Hash.BLAKE2B) (Irmin.Contents.String) (Irmin.Branch.String)
open Rpc

type config = {
  level : Logs.level;
  port : int;
  addr : string;
  unix_socket : string option;
  tls : bool;
}

let help () = Printf.printf "See output of `%s --help` for usage\n" Sys.argv.(0)

let init ~addr ~port ~unix_socket ~ssl ~level =
  let () = Logs.set_level (Logs.level_of_string level |> Result.get_ok) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let config = Client.conf ~addr ~port ?unix_socket ~tls:ssl () in
  Client.connect config

let run = Lwt_main.run

let ping client =
  run
    ( client >>= fun client ->
      let+ result = Client.ping client in
      let () = Error.unwrap "ping" result in
      Logs.app (fun l -> l "OK") )

let find client key =
  run
    ( client >>= fun client ->
      let* result = Client.Store.find client key >|= Error.unwrap "find" in
      match result with
      | Some data -> Lwt_io.printl data
      | None ->
          Logs.err (fun l ->
              l "Not found: %a" (Irmin.Type.pp Server.Store.key_t) key);
          Lwt.return_unit )

let set client key author message contents =
  run
    ( client >>= fun client ->
      let info = Irmin_unix.info ~author "%s" message in
      let+ () =
        Client.Store.set client key ~info contents >|= Error.unwrap "set"
      in
      Logs.app (fun l -> l "OK") )

let remove client key author message =
  run
    ( client >>= fun client ->
      let info = Irmin_unix.info ~author "%s" message in
      let+ () =
        Client.Store.remove client key ~info >|= Error.unwrap "remove"
      in
      Logs.app (fun l -> l "OK") )

let port =
  let doc = Arg.info ~doc:"Port to listen on" [ "p"; "port" ] in
  Arg.(value @@ opt int 8888 doc)

let addr =
  let doc = Arg.info ~doc:"Address to listen on" [ "addr"; "a" ] in
  Arg.(value @@ opt string "127.0.0.1" doc)

let level =
  let doc = Arg.info ~doc:"Log level" [ "log-level" ] in
  Arg.(value @@ opt string "error" doc)

let pr_str = Format.pp_print_string

let key index =
  let path_conv =
    let parse str =
      let x = Irmin.Type.of_string Server.Store.key_t str |> Result.get_ok in
      `Ok x
    in
    let print ppf path =
      pr_str ppf (Irmin.Type.to_string Server.Store.key_t path)
    in
    (parse, print)
  in
  let doc = Arg.info ~docv:"PATH" ~doc:"Key to lookup or modify." [] in
  Arg.(required & pos index (some path_conv) None & doc)

let author =
  let doc = Arg.info ~docv:"NAME" ~doc:"Commit author name" [ "author" ] in
  Arg.(value & opt string "irmin-client" & doc)

let message =
  let doc = Arg.info ~docv:"MESSAGE" ~doc:"Commit message" [ "message" ] in
  Arg.(value & opt string "" & doc)

let contents index =
  let doc = Arg.info ~docv:"DATA" ~doc:"Contents" [] in
  Arg.(required & pos index (some string) None & doc)

let unix_socket =
  let doc = Arg.info ~doc:"Unix domain socket to connect to" [ "unix"; "u" ] in
  Arg.(value @@ opt (some string) None doc)

let ssl =
  let doc = Arg.info ~doc:"Enable SSL" [ "ssl" ] in
  Arg.(value @@ flag doc)

let config =
  let create addr port unix_socket ssl level =
    init ~addr ~port ~unix_socket ~ssl ~level
  in
  Term.(const create $ addr $ port $ unix_socket $ ssl $ level)

let ping = (Term.(const ping $ config), Term.info "ping")

let get = (Term.(const find $ config $ key 0), Term.info "get")

let find = (Term.(const find $ config $ key 0), Term.info "find")

let remove =
  Term.(const remove $ config $ key 0 $ author $ message, Term.info "remove")

let set =
  Term.
    (const set $ config $ key 0 $ author $ message $ contents 1, Term.info "set")

let help = (Term.(const help $ Term.pure ()), Term.info "irmin-client")

let () = Term.exit @@ Term.eval_choice help [ ping; get; find; set; remove ]
