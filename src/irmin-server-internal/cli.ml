open Cmdliner

let default_uri = Uri.of_string "tcp://127.0.0.1:9181"

let uri : Uri.t option Cmdliner.Term.t =
  let doc =
    Arg.info ~docv:"URL" ~doc:"URI to connect to or listen on" [ "uri"; "u" ]
  in
  Term.(
    const (Option.map Uri.of_string)
    $ Arg.(value & opt (some string) None & doc))

let config_path : string option Cmdliner.Term.t =
  let doc = Arg.info ~docv:"PATH" ~doc:"Config path" [ "config" ] in
  Arg.(value & opt (some string) None & doc)

let codec =
  let open Conn.Codec in
  let doc = Arg.info ~doc:"Encoding to use for messages" [ "codec" ] in
  let t = Arg.enum [ ("bin", (module Bin : S)); ("json", (module Json : S)) ] in
  Arg.(value & opt t (module Bin) doc)
