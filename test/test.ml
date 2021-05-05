open Lwt.Syntax
open Lwt.Infix
open Irmin_server
include Util

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping client =
  let open Rpc.Client in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let set client =
  let open Rpc.Client in
  let info = Info.v "test: set" in
  let* r = Store.set ~info client [ "a"; "b"; "c" ] "123" in
  let () = Alcotest.(check (result unit error)) "set" (Ok ()) r in
  let+ r2 = Store.find client [ "a"; "b"; "c" ] in
  Alcotest.(check (result (option string) error)) "get" (Ok (Some "123")) r2

let get_missing client =
  let open Rpc.Client in
  let+ r = Store.find client [ "missing" ] in
  Alcotest.(check (result (option string) error)) "get_missing" (Ok None) r

let tree client =
  let open Rpc.Client in
  let tree = Tree.empty client in
  let* local = Tree.to_local tree >|= Error.unwrap "local" in
  Alcotest.(check (ty Tree.Local.t)) "empty tree" Tree.Local.empty local;
  let* tree = Tree.add tree [ "x" ] "foo" >|= Error.unwrap "x" in
  let* tree = Tree.add tree [ "y" ] "bar" >|= Error.unwrap "y" in
  let* local = Tree.to_local tree >|= Error.unwrap "local x, y" in
  let* local' =
    Tree.Local.(add empty [ "x" ] "foo" >>= fun x -> add x [ "y" ] "bar")
  in
  Alcotest.(check (ty Tree.Local.t)) "x, y" local' local;
  let+ res = Store.set_tree ~info:(Info.v "set_tree") client [ "tree" ] tree in
  Alcotest.(check bool "set_tree") true (Result.is_ok res)

let branch (client : Rpc.Client.t) =
  let open Rpc.Client in
  let module Store = Rpc.Server.Store in
  let* current = Branch.get_current client in
  Alcotest.(check (result string error))
    "current branch is master" (Ok Branch.master) current;
  let* () = Branch.set_current client "test" >|= Error.unwrap "set_current" in
  let* current = Branch.get_current client in
  Alcotest.(check (result string error)) "current branch" (Ok "test") current;
  let* () = Branch.remove client "test" >|= Error.unwrap "remove" in
  let* current = Branch.get_current client in
  Alcotest.(check (result string error))
    "current branch is master again" (Ok Branch.master) current;
  let* _ = Rpc.Client.Store.set ~info:(Info.v "test") client [ "test" ] "ok" in
  let* head = Branch.get client >|= Error.unwrap "get" in
  let head = Option.get head in
  let tree = Commit.tree client head in
  let hash = Commit.hash head in
  let* commit =
    Commit.v client ~info:(Info.v "test") ~parents:[ hash ] tree
    >|= Error.unwrap "Commit.create"
  in
  let* () = Branch.set client commit >|= Error.unwrap "set" in
  let+ head = Branch.get client >|= Error.unwrap "get" in
  Alcotest.(check string)
    "commit info" "test"
    (Commit.info (Option.get head) |> Info.message)

let all =
  [
    ("ping", `Quick, ping);
    ("set", `Quick, set);
    ("get_missing", `Quick, get_missing);
    ("tree", `Quick, tree);
    ("branch", `Quick, branch);
  ]

let () =
  Lwt_main.run
    (let wake, uri = run_server () in
     let* () = Lwt_unix.sleep 1. in
     let* client = Rpc.Client.connect ~uri () in
     Logs.debug (fun l -> l "Connected");
     let+ () = Alcotest_lwt.run "irmin-server" [ ("all", suite client all) ] in
     Lwt.wakeup wake ())
