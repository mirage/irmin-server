open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Server = Irmin_server.Make (Store)

let info message () =
  Store.Info.v ~author:"irmin-server" ~message
    (Unix.gettimeofday () |> Int64.of_float)

let add_key_value repo =
  let* main = Store.main repo in
  Store.set_exn ~info:(info "Initial commit") main [ "hello" ] "world"

let usage_msg = "server --uri <uri> --with-commit"

let main =
  let with_commit = ref false in
  let uri = ref (Uri.to_string Utils.default_uri) in
  let speclist =
    [
      ( "--with-commit",
        Arg.Set with_commit,
        "Add a single commit with key [ \"hello\" ] and value \"world\"" );
      ("--uri", Arg.Set_string uri, "The URI for the server to listen on");
    ]
  in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let config = Irmin_mem.config () in
  let* repo = Store.Repo.v config in
  let* server = Server.v ~uri:(Uri.of_string !uri) config in
  let* () = if !with_commit then add_key_value repo else Lwt.return_unit in
  let () =
    Fmt.pr "Listening on %a@.Store empty: %b@." Uri.pp (Uri.of_string !uri)
      (not !with_commit)
  in
  Server.serve server

let () = Lwt_main.run main
