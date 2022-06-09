open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)
module Error = Irmin_client.Error

let main =
  let info () = Client.Info.empty in
  let uri = Utils.get_url in
  let* client = Client.connect uri in
  let* tree = Client.Batch.Tree.empty client in
  let* tree = Client.Batch.Tree.add client tree [ "b"; "c" ] "123" in

  let batch = Client.Batch.v () in
  let batch = Client.Batch.add batch [ "foo" ] "bar" in
  let batch = Client.Batch.add_tree batch [ "a" ] tree in
  let batch = Client.Batch.remove batch [ "testing" ] in

  let* main = Client.main client in
  let* () = Client.set_exn ~info main [ "testing" ] "testing" in
  let* () = Client.Batch.commit ~info main [] batch in

  let* foo = Client.get main [ "foo" ] in
  let* abc = Client.get main [ "a"; "b"; "c" ] in
  let* testing = Client.find main [ "testing" ] in
  assert (foo = "bar");
  assert (abc = "123");
  assert (Option.is_none testing);
  Lwt_io.printlf "foo => %s\na/b/c => %s" foo abc

let () = Lwt_main.run main
