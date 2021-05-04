open Cmdliner

let uri : string Cmdliner.Term.t =
  let doc =
    Arg.info ~docv:"URL" ~doc:"URI to connect to or listen on" [ "uri"; "u" ]
  in
  Arg.(value & opt string "tcp://127.0.0.1:8888" & doc)

let contents = Irmin_unix.Resolver.Contents.term

let hash = Irmin_unix.Resolver.Hash.term

let store = Irmin_unix.Resolver.Store.term

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
