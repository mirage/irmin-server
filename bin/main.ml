open Lwt.Syntax
module Rpc = Irmin_server.Make (Irmin.Hash.BLAKE2B) (Irmin.Contents.String)

let main =
  let open Rpc in
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "client" then (
    let conf = Client.conf ~port:8888 () in
    let* client = Client.connect conf in
    let* x = Client.ping client in
    assert (Result.is_ok x);
    let+ x = Client.ping client in
    assert (Result.is_ok x))
  else
    let config = Irmin_pack.config "./data" in
    let* server = Server.v ~port:8888 config in
    Server.serve server

let () = Lwt_main.run main
