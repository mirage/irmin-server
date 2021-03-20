open Bench_common
open Lwt.Syntax
open Lwt.Infix
open Irmin_server

let unwrap = Irmin_server.Error.unwrap

module type BENCH = sig
  type tree

  module Tree : sig
    val add : tree -> string list -> string -> tree Error.result Lwt.t
  end
end

module Make (X : BENCH) = struct
  let rec add_n client tree n =
    if n = 0 then Lwt.return tree
    else
      let s = random_string 512 in
      let key = [ string_of_int n ] in
      let* tree = X.Tree.add tree key s >|= unwrap "add" in
      add_n client tree (n - 1)
end

let run_remote uri count commits tls hash =
  let (module Hash : Irmin.Hash.S) =
    Option.value ~default:Cli.default_hash hash
  in
  let module Rpc =
    Irmin_server.Make (Hash) (Irmin.Contents.String) (Irmin.Branch.String)
  in
  let module Remote = Make (Rpc.Client) in
  let+ n, () =
    let* client = Rpc.Client.connect ~tls ~uri () in
    let* () = Rpc.Client.ping client >|= unwrap "ping" in
    let rec aux commits =
      if commits = 0 then Lwt.return_unit
      else
        let* tree = Rpc.Client.Tree.empty client >|= unwrap "rpc" in
        Logs.app (fun l -> l "Adding items to tree (commit %d)" commits);
        let* n, tree = with_timer (fun () -> Remote.add_n client tree count) in

        Logs.app (fun l ->
            l "Items added per second: %f" (float_of_int count /. n));

        Logs.app (fun l -> l "Setting tree");
        let* _ =
          Rpc.Client.Store.set_tree client ~info:(Irmin_unix.info "test")
            [ "a" ] tree
          >|= unwrap "set_tree"
        in
        Logs.app (fun l -> l "Done (commit %d)" commits);
        aux (commits - 1)
    in

    with_timer (fun () -> aux commits)
  in
  Logs.app (fun l -> l "%f" n)

let run_direct root count commits hash =
  let (module Hash : Irmin.Hash.S) =
    Option.value ~default:Cli.default_hash hash
  in
  let module Rpc =
    Irmin_server.Make (Hash) (Irmin.Contents.String) (Irmin.Branch.String)
  in
  let module Store = Rpc.Server.Store in
  let module Direct = Make (struct
    type tree = Store.tree

    module Tree = struct
      let add t k v = Store.Tree.add t k v >>= Lwt.return_ok
    end
  end) in
  let+ n, () =
    let config = Irmin_pack.config root in
    let* repo = Store.Repo.v config in
    let* master = Store.master repo in

    let rec aux commits =
      if commits = 0 then Lwt.return_unit
      else
        let tree = Store.Tree.empty in
        Logs.app (fun l -> l "Adding items to tree (commit %d)" commits);
        let* n, tree = with_timer (fun () -> Direct.add_n () tree count) in

        Logs.app (fun l ->
            l "Items added per second: %f" (float_of_int count /. n));
        Logs.app (fun l -> l "Setting tree");
        let* () =
          Store.set_tree_exn master ~info:(Irmin_unix.info "test") [ "a" ] tree
        in
        Logs.app (fun l -> l "Done setting tree (commit %d)" commits);
        aux (commits - 1)
    in

    with_timer (fun () -> aux commits)
  in
  Logs.app (fun l -> l "Time: %f" n)

open Cmdliner

let iterations =
  let doc =
    Arg.info ~doc:"Number of iterations (Tree.add)" [ "i"; "iterations" ]
  in
  Arg.(value @@ opt int 100_000 doc)

let commits =
  let doc =
    Arg.info ~doc:"Number of commits (Store.set_tree)" [ "c"; "commits" ]
  in
  Arg.(value @@ opt int 3 doc)

let uri =
  let doc = Arg.info ~docv:"URL" ~doc:"URI to connect to" [ "uri"; "u" ] in
  Arg.(value & opt string "tcp://127.0.0.1:8888" & doc)

let direct =
  let doc =
    Arg.info ~doc:"Run directly against disk (without network)" [ "direct" ]
  in
  Arg.(value & flag doc)

let tls =
  let doc = Arg.info ~doc:"Enable TLS" [ "tls" ] in
  Arg.(value @@ flag doc)

let main uri iterations commits direct tls hash log_level =
  Logs.set_level (Some log_level);
  Logs.set_reporter (reporter ());
  Logs.app (fun l ->
      l "Running benchmark with %d iterations and %d commits" iterations commits);
  Lwt_main.run
  @@
  if direct then run_direct "./data" iterations commits hash
  else run_remote uri iterations commits tls hash

let main_term =
  Term.(
    const main $ uri $ iterations $ commits $ direct $ tls
    $ Irmin_server.Cli.hash $ Irmin_server.Cli.log_level)

let () =
  let () = Memtrace.trace_if_requested () in
  let info = Term.info "irmin-bench-basic" in
  Term.exit @@ Term.eval (main_term, info)
