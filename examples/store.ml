open Lwt.Syntax
module S = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (S)

let main =
  let uri = Utils.get_url in
  let* repo = Client.connect uri in
  let* t = Client.main repo in
  let* w = Client.watch t (fun _ -> Lwt_io.printl "UPDATED") in
  let* () = Client.set_exn t ~info:Client.Info.none [ "a"; "b"; "c" ] "123" in
  let* x = Client.get t [ "a"; "b"; "c" ] in
  let* () = Client.unwatch w in
  Lwt_io.printl x

let () = Lwt_main.run main
