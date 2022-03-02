open Lwt.Syntax
open Lwt.Infix
open Irmin_client_unix
open Util
module Info = Info (Client.Info)

let info = Info.v

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
  let open Client in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let set client =
  let open Client in
  let info = info "test: set" in
  let* r = set ~info client [ "a"; "b"; "c" ] "123" in
  let () = Alcotest.(check (result unit error)) "set" (Ok ()) r in
  let+ r2 = find client [ "a"; "b"; "c" ] in
  Alcotest.(check (result (option string) error)) "get" (Ok (Some "123")) r2

let get_missing client =
  let open Client in
  let+ r = find client [ "missing" ] in
  Alcotest.(check (result (option string) error)) "get_missing" (Ok None) r

let tree client =
  let open Client in
  let tree = Tree.empty client in
  let* local = Tree.to_local tree >|= Error.unwrap "local" in
  Alcotest.(check (ty Tree.Local.t)) "empty tree" (Tree.Local.empty ()) local;
  let* tree = Tree.add tree [ "x" ] "foo" >|= Error.unwrap "x" in
  let* tree = Tree.add tree [ "y" ] "bar" >|= Error.unwrap "y" in
  let* local = Tree.to_local tree >|= Error.unwrap "local x, y" in
  let* local' =
    Tree.Local.(add (empty ()) [ "x" ] "foo" >>= fun x -> add x [ "y" ] "bar")
  in
  Alcotest.(check (ty Tree.Local.t)) "x, y" local' local;
  let* res = set_tree ~info:(info "set_tree") client [ "tree" ] tree in
  Alcotest.(check bool "set_tree") true (Result.is_ok res);
  let* tree = find_tree client Path.empty >|= Error.unwrap "find_tree" in
  let tree = Option.get tree in
  let+ res = set_tree ~info:(info "set_tree") client [ "tree" ] tree in
  Alcotest.(check bool "set_tree") true (Result.is_ok res)

let branch (client : Client.t) =
  let open Client in
  let module Store = Server.Store in
  let* current = Branch.get_current client in
  Alcotest.(check (result string error))
    "current branch is main" (Ok Branch.main) current;
  let* () = Branch.set_current client "test" >|= Error.unwrap "set_current" in
  let* current = Branch.get_current client in
  Alcotest.(check (result string error)) "current branch" (Ok "test") current;
  let* () = Branch.remove client "test" >|= Error.unwrap "remove" in
  let* current = Branch.get_current client in
  Alcotest.(check (result string error))
    "current branch is main again" (Ok Branch.main) current;
  let* _ = Client.set ~info:(info "test") client [ "test" ] "ok" in
  let* head = Branch.get client >|= Error.unwrap "get" in
  let head = Option.get head in
  let tree = Commit.tree client head in
  let hash = Commit.key head in
  let* commit =
    Commit.v client ~info:(info "test") ~parents:[ hash ] tree
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
     let* client = Client.connect ~uri () in
     Logs.debug (fun l -> l "Connected");
     let+ () = Alcotest_lwt.run "irmin-server" [ ("all", suite client all) ] in
     Lwt.wakeup wake ())
