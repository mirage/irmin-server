open Cmdliner

let uri : string Cmdliner.Term.t =
  let doc =
    Arg.info ~docv:"URL" ~doc:"URI to connect to or listen on" [ "uri"; "u" ]
  in
  Arg.(value & opt string "tcp://127.0.0.1:8888" & doc)

let log_level =
  let level =
    let parse str =
      match Logs.level_of_string str with
      | Ok x -> `Ok x
      | Error (`Msg s) -> `Error s
    in
    let print ppf path =
      Format.pp_print_string ppf (Logs.level_to_string path)
    in
    (parse, print)
  in
  let doc = Arg.info ~docv:"PATH" ~doc:"Key to lookup or modify." [] in
  Arg.(required & opt level (Some Logs.App) & doc)
