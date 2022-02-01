open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Server = Irmin_server.Make (Store)

let main =
  let config = Irmin_mem.config () in
  let uri = Uri.of_string "tcp://localhost:9090" in
  let* server = Server.v ~uri config in
  let () = Format.printf "Listening on %a@." Uri.pp uri in
  Server.serve server

let () = Lwt_main.run main
