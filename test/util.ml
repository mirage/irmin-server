open Lwt.Infix
open Irmin_server
module Rpc = KV (Irmin.Contents.String)

let test name f client _switch () =
  Logs.debug (fun l -> l "Running: %s" name);
  f client

let run_server () =
  let path = Unix.getcwd () in
  let uri = "unix://" ^ Filename.concat path "test.socket" in
  Lwt.async (fun () ->
      let conf = Irmin_pack.config "test-db" in
      Rpc.Server.v ~uri conf >>= Rpc.Server.serve);
  uri

let suite client all =
  List.map
    (fun (name, speed, f) ->
      Alcotest_lwt.test_case name speed (test name f client))
    all
