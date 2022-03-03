open Lwt.Infix
module Codec = Irmin_server_internal.Conn.Codec.Bin
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make_ext (Codec) (Store)
module Server = Irmin_server.Make_ext (Codec) (Store)

let test name f client _switch () =
  Logs.debug (fun l -> l "Running: %s" name);
  f client

let run_server () =
  let uri = Uri.of_string "tcp://127.0.0.1:12345" in
  match Lwt_unix.fork () with
  | 0 ->
      let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
      let conf = Irmin_mem.config () in
      Lwt_main.run (Server.v ~uri conf >>= Server.serve);
      (0, uri)
  | n -> (n, uri)

let suite client all =
  List.map
    (fun (name, speed, f) ->
      Alcotest_lwt.test_case name speed (test name f client))
    all
