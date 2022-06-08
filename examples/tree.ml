open Lwt.Syntax
open Lwt.Infix
open Irmin_client_unix
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Make (Store)
module Info = Info (Client.Info)

let main =
  let uri = Utils.get_url in
  let* repo = Client.connect uri in
  let* client = Client.main repo in

  (* Create an empty tree. This tree will remain on the client-side until
     committed to the store. *)
  let tree = Client.Tree.empty () in

  (* If needed, [Client.Tree.cleanup] can be used to manually garbage collect
     a tree *)

  (* Set foo => bar *)
  let* tree = Client.Tree.add tree [ "foo" ] "bar" in

  (* Check that the tree has been updated *)
  let* exists = Client.Tree.mem tree [ "foo" ] in
  assert exists;

  (* Commit the tree *)
  let info = Info.v "set_tree" in

  (* Get the tree back, now instead of existing on the client-side, you get a
     key to the stored tree *)
  let* _ = Client.set_tree client ~info [] tree >|= Result.get_ok in

  (* Check to make sure the store contains foo => bar *)
  let+ value = Client.find client [ "foo" ] in
  let value = Option.get value in
  assert (value = "bar")

let () = Lwt_main.run main
