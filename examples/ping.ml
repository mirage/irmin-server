open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let main =
  let tcp = Uri.of_string "tcp://localhost:9090" in
  let uri = try if Sys.argv.(1) = "ws" then Uri.of_string "ws://localhost:9090/ws" else tcp with _ -> tcp in
  let* client = Client.connect ~uri () in
  let+ res = Client.ping client in
  match res with
  | Ok () -> print_endline "OK"
  | Error e -> Printf.printf "ERROR: %s\n" (Irmin_client.Error.to_string e)

let () = Lwt_main.run main
