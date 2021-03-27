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

let stream_is_empty stream =
  try
    Stream.empty stream;
    true
  with Stream.Failure -> false

let key ?(index = 1) item =
  Yojson.Safe.Util.index index item
  |> Yojson.Safe.Util.to_list
  |> List.map Yojson.Safe.Util.to_string

let load_trace (type a)
    (module Rpc : Irmin_server.S
      with type Client.t = a
       and type Store.contents = string) (client : a) max =
  let* tree = Rpc.Client.Tree.empty client >|= Error.unwrap "tree" in
  let stream = Dataset.get_stream () in
  let count = ref 0 in
  let hashmap = Hashtbl.create 8 in
  let rec aux client stream tree =
    if stream_is_empty stream then Lwt.return_unit
    else if max > 0 && !count >= max then Lwt.return_unit
    else
      let () =
        if !count mod 10000 = 0 then
          Logs.app (fun l -> l "Executed %d operations" (Stream.count stream))
      in
      let item = Stream.next stream in
      let command =
        Yojson.Safe.Util.index 0 item |> Yojson.Safe.Util.to_string
      in

      Logs.info (fun l -> l "Command: %s" command);
      (match command with
      | "Add" ->
          let key = key item in
          let value =
            Yojson.Safe.Util.index 2 item |> Yojson.Safe.Util.to_string
          in
          Rpc.Client.Tree.add tree key value >|= Error.unwrap "trace.Add"
      | "Find" ->
          let key = key item in
          let _result =
            Yojson.Safe.Util.index 2 item |> Yojson.Safe.Util.to_bool
          in
          let+ _result' =
            Rpc.Client.Tree.find tree key >|= Error.unwrap "trace.Find"
          in
          (*assert (Option.is_some result' = result);*)
          tree
      | "Mem" ->
          let key = key item in
          let _result =
            Yojson.Safe.Util.index 2 item |> Yojson.Safe.Util.to_bool
          in
          let+ _result' =
            Rpc.Client.Tree.mem tree key >|= Error.unwrap "trace.Mem"
          in
          (*assert (result' = result);*)
          tree
      | "Mem_tree" ->
          let key = key item in
          let result =
            Yojson.Safe.Util.index 2 item |> Yojson.Safe.Util.to_bool
          in
          let+ result' =
            Rpc.Client.Tree.mem_tree tree key >|= Error.unwrap "trace.Mem_tree"
          in
          assert (result' = result);
          tree
      | "Copy" -> (
          let a = key item in
          let b = key ~index:2 item in
          let* a =
            Rpc.Client.Tree.find_tree tree a >|= Error.unwrap "trace.Copy"
          in
          match a with
          | Some a ->
              Rpc.Client.Tree.add_tree tree b a
              >|= Error.unwrap "trace.Copy (add)"
          | None -> Lwt.return tree)
      | "Remove" ->
          let key = key item in
          Rpc.Client.Tree.remove tree key >|= Error.unwrap "trace.Remove"
      | "Commit" ->
          let hash =
            Yojson.Safe.Util.index 1 item |> Yojson.Safe.Util.to_string
          in
          let date =
            Yojson.Safe.Util.index 2 item
            |> Yojson.Safe.Util.to_int |> Int64.of_int
          in
          let message =
            Yojson.Safe.Util.index 3 item |> Yojson.Safe.Util.to_string
          in
          let info = Irmin.Info.v ~date ~author:"irmin-server" message in
          let* parent =
            Rpc.Client.Branch.get client >|= Error.unwrap "Branch.get"
          in
          let parents =
            try [ Rpc.Client.Commit.node (Option.get parent) ] with _ -> []
          in
          let* (commit : Rpc.Client.Commit.t) =
            Rpc.Client.Commit.create client ~info:(fun () -> info) ~parents tree
            >|= Error.unwrap "trace.Commit"
          in
          Hashtbl.replace hashmap hash (Rpc.Client.Commit.node commit);
          let+ () =
            Rpc.Client.Branch.set client commit >|= Error.unwrap "branch.set"
          in
          tree
      | "Checkout" ->
          let hash =
            Yojson.Safe.Util.index 1 item |> Yojson.Safe.Util.to_string
          in
          let hash = Hashtbl.find hashmap hash in
          let* commit =
            Rpc.Client.Commit.of_hash client hash >|= Error.unwrap "of_hash"
          in
          Rpc.Client.Commit.tree client (Option.get commit)
          >|= Error.unwrap "tree"
      | s ->
          Logs.app (fun l -> l "Unknown command: %s" s);
          Lwt.return tree)
      >>= fun tree ->
      incr count;
      (aux [@tailcall]) client stream tree
  in
  aux client stream tree >|= fun () -> !count

let run_remote uri max tls hash =
  let (module Hash : Irmin.Hash.S) =
    Option.value ~default:Cli.default_hash hash
  in
  let module Rpc =
    Irmin_server.Make (Hash) (Irmin.Contents.String) (Irmin.Branch.String)
  in
  let+ n, count =
    let* client = Rpc.Client.connect ~tls ~uri () in
    with_timer (fun () -> load_trace (module Rpc) client max)
  in
  Logs.app (fun l ->
      l "%d trace operations executed in %fs (%f/s)" count n
        (float_of_int count /. n))

open Cmdliner

let max =
  let doc = Arg.info ~doc:"Max number of operations" [ "m"; "max" ] in
  Arg.(value @@ opt int 0 doc)

let uri =
  let doc = Arg.info ~docv:"URL" ~doc:"URI to connect to" [ "uri"; "u" ] in
  Arg.(value & opt string "tcp://127.0.0.1:8888" & doc)

let tls =
  let doc = Arg.info ~doc:"Enable TLS" [ "tls" ] in
  Arg.(value @@ flag doc)

let main uri operations tls hash log_level =
  Logs.set_level (Some log_level);
  Logs.set_reporter (reporter ());
  Logs.app (fun l ->
      l "Running benchmark with %s operations"
        (if operations <= 0 then "all" else string_of_int operations));
  Lwt_main.run @@ run_remote uri operations tls hash

let main_term =
  Term.(
    const main $ uri $ max $ tls $ Irmin_server.Cli.hash
    $ Irmin_server.Cli.log_level)

let () =
  let () = Memtrace.trace_if_requested () in
  let info = Term.info "irmin-server-bench" in
  Term.exit @@ Term.eval (main_term, info)
