open Lwt.Syntax
module S = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make (S)
module Store = Irmin_client_jsoo.Store.Make (S)

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let display_text result = 
  let open Brr in
  let result_element =  (Document.find_el_by_id G.document) (Jstr.v "result") in
  match result_element with
  | Some elem -> El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v result) elem
  | None -> ()

let ping () =
  display_text "";
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let* client = Client.connect ~uri () in
  let+ res = Client.ping client in
  match res with
  | Ok () -> display_text "OK"
  | Error e -> Printf.printf "ERROR: %s\n" (Irmin_client.Error.to_string e)

let send_data () =
  display_text "";
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let config = Irmin_client_jsoo.Store.config uri in
  let* repo = Store.Repo.v config in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:Store.Info.none [ "a"; "b"; "c" ] "123" in
  let+ x = Store.get t [ "a"; "b"; "c" ] in
  display_text x

let () =
  let open Brr in
  let ping_btn = (Document.find_el_by_id G.document) (Jstr.v "ping") in
  (Option.iter (fun el -> Ev.listen Ev.click (fun _ -> Lwt.async ping) (El.as_target el)) ping_btn);
  let senddata_btn = (Document.find_el_by_id G.document) (Jstr.v "senddata") in
  (Option.iter (fun elem -> Ev.listen Ev.click (fun _ -> Lwt.async send_data) (El.as_target elem)) senddata_btn)