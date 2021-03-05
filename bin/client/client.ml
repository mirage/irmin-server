open Cmdliner
open Lwt.Syntax
open Lwt.Infix
open Irmin_server

let help () = Printf.printf "See output of `%s --help` for usage\n" Sys.argv.(0)

type client = S : ((module Client.S with type t = 'a) * 'a Lwt.t) -> client

let init ~uri ~tls ~level (module Rpc : S) : client =
  let () = Logs.set_level (Some level) in
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let (x : Rpc.Client.t Lwt.t) = Rpc.Client.connect ~tls ~uri () in
  S ((module Rpc.Client : Client.S with type t = Rpc.Client.t), x)

let run = Lwt_main.run

let ping (S ((module Client), client)) =
  run
    ( client >>= fun client ->
      let+ result = Client.ping client in
      let () = Error.unwrap "ping" result in
      Logs.app (fun l -> l "OK") )

let find (S ((module Client), client)) key =
  run
    ( client >>= fun client ->
      let key = Irmin.Type.of_string Client.Key.t key |> Error.unwrap "key" in
      let* result = Client.Store.find client key >|= Error.unwrap "find" in
      match result with
      | Some data -> Lwt_io.printl (Irmin.Type.to_string Client.Contents.t data)
      | None ->
          Logs.err (fun l -> l "Not found: %a" (Irmin.Type.pp Client.Key.t) key);
          Lwt.return_unit )

let set (S ((module Client), client)) key author message contents =
  run
    ( client >>= fun client ->
      let key = Irmin.Type.of_string Client.Key.t key |> Error.unwrap "key" in
      let contents =
        Irmin.Type.of_string Client.Contents.t contents
        |> Error.unwrap "contents"
      in
      let info = Irmin_unix.info ~author "%s" message in
      let+ () =
        Client.Store.set client key ~info contents >|= Error.unwrap "set"
      in
      Logs.app (fun l -> l "OK") )

let remove (S ((module Client), client)) key author message =
  run
    ( client >>= fun client ->
      let key = Irmin.Type.of_string Client.Key.t key |> Error.unwrap "key" in
      let info = Irmin_unix.info ~author "%s" message in
      let+ () =
        Client.Store.remove client key ~info >|= Error.unwrap "remove"
      in
      Logs.app (fun l -> l "OK") )

let level =
  let doc = Arg.info ~doc:"Log level" [ "log-level" ] in
  Arg.(value @@ opt string "error" doc)

let pr_str = Format.pp_print_string

let key index =
  let doc = Arg.info ~docv:"PATH" ~doc:"Key to lookup or modify." [] in
  Arg.(required & pos index (some string) None & doc)

let author =
  let doc = Arg.info ~docv:"NAME" ~doc:"Commit author name" [ "author" ] in
  Arg.(value & opt string "irmin-client" & doc)

let message =
  let doc = Arg.info ~docv:"MESSAGE" ~doc:"Commit message" [ "message" ] in
  Arg.(value & opt string "" & doc)

let value index =
  let doc = Arg.info ~docv:"DATA" ~doc:"Value" [] in
  Arg.(required & pos index (some string) None & doc)

let tls =
  let doc = Arg.info ~doc:"Enable TLS" [ "tls" ] in
  Arg.(value @@ flag doc)

let config =
  let create uri tls level contents hash =
    let (module Hash : Irmin.Hash.S) =
      Option.value ~default:Cli.default_hash hash
    in
    let contents =
      Irmin_unix.Resolver.Contents.find
        (Option.value ~default:Cli.default_contents contents)
    in
    let (module Contents : Irmin.Contents.S) = contents in
    let module Rpc = Irmin_server.Make (Hash) (Contents) (Irmin.Branch.String)
    in
    init ~uri ~tls ~level (module Rpc)
  in
  Term.(const create $ Cli.uri $ tls $ Cli.log_level $ Cli.contents $ Cli.hash)

let ping = (Term.(const ping $ config), Term.info "ping")

let get = (Term.(const find $ config $ key 0), Term.info "get")

let find = (Term.(const find $ config $ key 0), Term.info "find")

let remove =
  Term.(const remove $ config $ key 0 $ author $ message, Term.info "remove")

let set =
  Term.(const set $ config $ key 0 $ author $ message $ value 1, Term.info "set")

let help = (Term.(const help $ Term.pure ()), Term.info "irmin-client")

let () = Term.exit @@ Term.eval_choice help [ ping; get; find; set; remove ]
