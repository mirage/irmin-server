open Lwt.Syntax
module S = Irmin_mem.KV.Make (Irmin.Contents.String)
module Store = Irmin_client_jsoo.Make (S)

let display_text result =
  let open Brr in
  let result_element = (Document.find_el_by_id G.document) (Jstr.v "result") in
  match result_element with
  | Some elem ->
      El.set_prop (El.Prop.jstr (Jstr.v "innerHTML")) (Jstr.v result) elem
  | None -> ()

let config = Irmin_client_jsoo.config Utils.Util.server_uri

let ping () =
  display_text "";
  let* client = Store.Repo.v config in
  let+ res = Store.ping client in
  match res with
  | Ok () -> display_text "OK"
  | Error e -> display_text (Irmin_client.Error.to_string e)

let send_data () =
  display_text "";
  let* repo = Store.Repo.v config in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:Store.Info.none [ "a"; "b"; "c" ] "123" in
  let+ x = Store.get t [ "a"; "b"; "c" ] in
  display_text x

let () =
  let open Brr in
  let ping_btn = (Document.find_el_by_id G.document) (Jstr.v "ping") in
  Option.iter
    (fun el -> Ev.listen Ev.click (fun _ -> Lwt.async ping) (El.as_target el))
    ping_btn;
  let senddata_btn = (Document.find_el_by_id G.document) (Jstr.v "senddata") in
  Option.iter
    (fun elem ->
      Ev.listen Ev.click (fun _ -> Lwt.async send_data) (El.as_target elem))
    senddata_btn
