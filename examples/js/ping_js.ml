open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make (Store)

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let main () =
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let* client = Client.connect ~uri () in
  let+ res = Client.ping client in
  match res with
  | Ok () -> print_endline "OK"
  | Error e -> Printf.printf "ERROR: %s\n" (Irmin_client.Error.to_string e)

let () =
  let open Brr in
  Ev.listen Ev.load (fun _ -> Lwt.async main) (Window.as_target G.window)
