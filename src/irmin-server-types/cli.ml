open Cmdliner

let default_uri = Uri.of_string "tcp://127.0.0.1:9181"

let uri : Uri.t option Cmdliner.Term.t =
  let doc =
    Arg.info ~docv:"URL" ~doc:"URI to connect to or listen on" [ "uri"; "u" ]
  in
  Term.(
    const (Option.map Uri.of_string)
    $ Arg.(value & opt (some string) None & doc))

let () =
  Irmin_unix.Resolver.Contents.(
    add "string" ~default:true (module Irmin.Contents.String));
  Irmin_unix.Resolver.Hash.(
    add "blake2b" ~default:true (module Irmin.Hash.BLAKE2B));
  Irmin_unix.Resolver.Hash.(
    add "tezos" ~default:false (module Tezos_context_hash_irmin.Encoding.Hash));
  Irmin_unix.Resolver.Store.(
    add "tezos"
      Irmin_unix.Resolver.Store.(
        (* Tezos store ignores content type argument *)
        Fixed_hash (fun _ -> v (module Tezos_context_hash_irmin.Store))));
  Irmin_unix.Resolver.Store.(add "pack" ~default:true (Variable_hash pack))

let store = Irmin_unix.Resolver.Store.term

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
