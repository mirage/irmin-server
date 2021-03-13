open Lwt.Syntax
open Lwt.Infix
open Irmin_server
module Rpc = KV (Irmin.Contents.String)

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ping client =
  let open Rpc.Client in
  let+ r = ping client in
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let set client =
  let open Rpc.Client in
  let info = Irmin_unix.info "test: set" in
  let* r = Store.set ~info client [ "a"; "b"; "c" ] "123" in
  let () = Alcotest.(check (result unit error)) "set" (Ok ()) r in
  let+ r2 = Store.find client [ "a"; "b"; "c" ] in
  Alcotest.(check (result (option string) error)) "get" (Ok (Some "123")) r2

let get_missing client =
  let open Rpc.Client in
  let+ r = Store.find client [ "missing" ] in
  Alcotest.(check (result (option string) error)) "get_missing" (Ok None) r

let test f client _switch () = f client

let all =
  [
    ("ping", `Quick, ping);
    ("set", `Quick, set);
    ("get_missing", `Quick, get_missing);
  ]

let run_server () =
  let path = Unix.getcwd () in
  let uri = "unix://" ^ Filename.concat path "test.socket" in
  Lwt.async (fun () ->
      let conf = Irmin_pack.config "test-db" in
      Rpc.Server.v ~uri conf >>= Rpc.Server.serve);
  uri

let () =
  Lwt_main.run
    (let uri = run_server () in
     let* () = Lwt_unix.sleep 1. in
     let* client = Rpc.Client.connect ~uri () in
     Alcotest_lwt.run "irmin-server"
       [
         ( "all",
           List.map
             (fun (name, speed, f) ->
               Alcotest_lwt.test_case name speed (test f client))
             all );
       ])
