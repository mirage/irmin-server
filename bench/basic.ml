open Bench_common
open Lwt.Syntax

(*open Lwt.Infix*)
module Store =
  Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)
module Rpc = Irmin_server.KV (Irmin.Contents.String)

let rec add master n =
  if n = 0 then Lwt.return_unit
  else
    let s = random_string (Random.int 1024) in
    let key = [ random_key () ] in
    let* _ = Rpc.Client.Store.set master key s ~info:(Irmin_unix.info "test") in
    add master (n - 1)

let rpc count =
  let+ n, () =
    with_timer (fun () ->
        let conf = Rpc.Client.conf ~port:8888 () in
        let* client = Rpc.Client.connect conf in
        let* () = add client count in
        Lwt.return_unit)
  in
  Logs.app (fun l -> l "%f" n)

let () = Lwt_main.run (rpc (try int_of_string Sys.argv.(1) with _ -> 10_000))
