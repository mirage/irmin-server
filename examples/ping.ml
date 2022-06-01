open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)

let main =
  let uri = Utils.Util.get_url in
  let config = Irmin_client_unix.config uri in
  let* client = Client.Repo.v config in
  let+ res = Client.ping client in
  match res with
  | Ok () -> print_endline "OK"
  | Error e -> Printf.printf "ERROR: %s\n" (Irmin_client.Error.to_string e)

let () = Lwt_main.run main
