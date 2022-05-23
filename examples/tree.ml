open Lwt.Syntax
open Lwt.Infix
open Irmin_client_unix
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Make (Store)
module Info = Info (Client.Info)

let main =
  let tcp = Uri.of_string "tcp://localhost:9090" in
  let uri =
    try
      if Sys.argv.(1) = "ws" then Uri.of_string "ws://localhost:9090/ws"
      else tcp
    with _ -> tcp
  in
  let* client = Client.connect ~uri () in

  (* Create an empty tree. This tree will remain on the client-side until
     committed to the store. *)
  let tree = Client.Tree.empty client in

  (* If needed, [Client.Tree.cleanup] can be used to manually garbage collect
     a tree *)

  (* Set foo => bar *)
  let* tree =
    Client.Tree.add tree [ "foo" ] "bar" >|= Error.unwrap "Tree.add"
  in

  (* Check that the tree has been updated *)
  let* exists = Client.Tree.mem tree [ "foo" ] >|= Error.unwrap "Tree.mem" in
  assert exists;

  (* Commit the tree *)
  let info = Info.v "set_tree" in

  (* Get the tree back, now instead of existing on the client-side, you get a
     key to the stored tree *)
  let* tree =
    Client.set_tree client ~info [] tree >|= Error.unwrap "Store.set_tree"
  in
  let _, t, _ = Client.Tree.split tree in
  let () =
    match t with
    | Client.Private.Tree.Key _ -> assert true
    | ID _ | Local _ -> assert false
  in

  (* Check to make sure the store contains foo => bar *)
  let+ value = Client.find client [ "foo" ] >|= Error.unwrap "Tree.find" in
  let value = Option.get value in
  assert (value = "bar")

let () = Lwt_main.run main
