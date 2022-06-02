open Lwt.Syntax
open Lwt.Infix
open Irmin_client_unix
open Util
module Info = Info (Client.Info)

let info = Info.v

let () =
  let style_renderer = `None in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter (Logs_fmt.reporter ())

module type R = sig
  val pid : int
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let () = at_exit (fun () -> Unix.kill R.pid Sys.sigint)
  let client = Lwt_main.run (Client.connect ~uri:R.uri ())
  let config = Irmin_client_unix.Store.config R.uri
  let client () = Client.dup client

  let clean ~config:_ =
    let* client = client () in
    Client.Branch.remove client "main" >|= Error.unwrap "remove"

  let init ~config:_ =
    let* client = client () in
    Client.Branch.remove client "main" >|= Error.unwrap "remove"

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Store.Make (X)

  let suite =
    Irmin_test.Suite.create ~name:R.kind ~init
      ~store:(module Store)
      ~config ~clean ()
end

let kind, pid, uri = run_server `Unix_domain

module Unix_socket = Make (struct
  let pid = pid
  let uri = uri
  let kind = kind
end)

module Tcp_socket = Make (struct
  let kind, pid, uri = run_server `Tcp
end)

module Websocket = Make (struct
  let kind, pid, uri = run_server `Websocket
end)

let client = Lwt_main.run (Client.connect ~uri ())
let client () = Client.dup client

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping () =
  let open Client in
  let* client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let set () =
  let open Client in
  let* client = client () in
  let info = info "test: set" in
  let* r = set ~info client [ "a"; "b"; "c" ] "123" in
  let () = Alcotest.(check (result unit error)) "set" (Ok ()) r in
  let+ r2 = find client [ "a"; "b"; "c" ] in
  Alcotest.(check (result (option string) error)) "get" (Ok (Some "123")) r2

let get_missing () =
  let open Client in
  let* client = client () in
  let+ r = find client [ "missing" ] in
  Alcotest.(check (result (option string) error)) "get_missing" (Ok None) r

let tree () =
  let open Client in
  let* client = client () in
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

let branch () =
  let open Client in
  let* client = client () in
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

let misc =
  let run f () = Lwt_main.run (f ()) in
  [
    ("ping", `Quick, run ping);
    ("set", `Quick, run set);
    ("get_missing", `Quick, run get_missing);
    ("tree", `Quick, run tree);
    ("branch", `Quick, run branch);
  ]

let misc = [ ("misc", misc) ]

let () =
  let slow = Sys.getenv_opt "SLOW" |> Option.is_some in
  let only = Sys.getenv_opt "ONLY" in
  let tests =
    match only with
    | Some "ws" -> [ (`Quick, Websocket.suite) ]
    | Some "tcp" -> [ (`Quick, Tcp_socket.suite) ]
    | Some "unix" -> [ (`Quick, Unix_socket.suite) ]
    | Some s -> failwith ("Invalid selection: " ^ s)
    | None ->
        [
          (`Quick, Unix_socket.suite);
          (`Quick, Tcp_socket.suite);
          (`Quick, Websocket.suite);
        ]
  in
  Irmin_test.Store.run "irmin-server" ~slow ~misc tests
