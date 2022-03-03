open Lwt.Syntax
module S = Irmin_mem.KV.Make (Irmin.Contents.String)
module Store = Irmin_client_unix.Store.Make (S)

let main =
  let uri = Uri.of_string "tcp://localhost:9090" in
  let config = Irmin_client_unix.Store.config uri in
  let* repo = Store.Repo.v config in
  let* t = Store.main repo in
  let* w = Store.watch t (fun _ -> Lwt_io.printl "UPDATED") in
  let* () = Store.set_exn t ~info:Store.Info.none [ "a"; "b"; "c" ] "123" in
  let* x = Store.get t [ "a"; "b"; "c" ] in
  let* () = Store.unwatch w in
  Lwt_io.printl x

let () = Lwt_main.run main
