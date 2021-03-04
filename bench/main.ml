open Bench_common
open Lwt.Syntax
open Lwt.Infix
module Rpc = Irmin_server.KV (Irmin.Contents.String)
module Client = Rpc.Client
module Store = Rpc.Server.Store

let unwrap = Irmin_server.Error.unwrap

module type BENCH = sig
  type t

  type tree

  module Tree : sig
    val add :
      t -> tree -> string list -> string -> tree Irmin_server.Error.result Lwt.t
  end
end

module Make (X : BENCH) = struct
  let rec add_n client tree n =
    if n = 0 then Lwt.return tree
    else
      let s = String.make 1024 'A' in
      let key = [ string_of_int n ] in
      let* tree = X.Tree.add client tree key s >|= unwrap "add" in
      add_n client tree (n - 1)
end

module Remote = Make (struct
  type tree = Rpc.Client.Tree.t

  include Rpc.Client
end)

module Direct = Make (struct
  type tree = Store.tree

  type t = unit

  module Tree = struct
    let add () t k v = Store.Tree.add t k v >>= Lwt.return_ok
  end
end)

let run_remote uri count commits tls =
  let+ n, () =
    let* client = Client.connect ~tls ~uri () in
    let rec aux commits =
      if commits = 0 then Lwt.return_unit
      else
        let* tree = Client.Tree.empty client >|= unwrap "rpc" in
        Logs.app (fun l -> l "Adding items to tree: commit# %d" commits);
        let* tree = Remote.add_n client tree count in

        Logs.app (fun l -> l "Setting tree");
        let* _ =
          Client.Store.set_tree client ~info:(Irmin_unix.info "test") [ "a" ]
            tree
          >|= unwrap "set_tree"
        in
        Logs.app (fun l -> l "Done setting tree: commit# %d" commits);
        aux (commits - 1)
    in

    with_timer (fun () -> aux commits)
  in
  Logs.app (fun l -> l "%f" n)

let run_direct root count commits =
  let+ n, () =
    let config = Irmin_pack.config root in
    let* repo = Store.Repo.v config in
    let* master = Store.master repo in

    let rec aux commits =
      if commits = 0 then Lwt.return_unit
      else
        let tree = Store.Tree.empty in
        Logs.app (fun l -> l "Adding items to tree: commit# %d" commits);
        let* tree = Direct.add_n () tree count in

        Logs.app (fun l -> l "Setting tree");
        let* () =
          Store.set_tree_exn master ~info:(Irmin_unix.info "test") [ "a" ] tree
        in
        Logs.app (fun l -> l "Done setting tree: commit# %d" commits);
        aux (commits - 1)
    in

    with_timer (fun () -> aux commits)
  in
  Logs.app (fun l -> l "%f" n)

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

let main uri iterations commits direct tls =
  Lwt_main.run
  @@
  if direct then run_direct "./data" iterations commits
  else run_remote uri iterations commits tls

let main_term = Term.(const main $ uri $ iterations $ commits $ direct $ tls)

let () =
  let info = Term.info "irmin-bench-basic" in
  Term.exit @@ Term.eval (main_term, info)
