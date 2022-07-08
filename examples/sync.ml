open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)
module Sync = Irmin.Sync.Make (Client)

let main =
  let uri = Utils.get_url in
  let* client = Client.connect uri in
  let* main = Client.main client in
  let* res = Client.ping client in
  match res with
  | Ok () -> (
      let* status =
        Sync.pull_exn main (Irmin.Sync.remote_store (module Client) main) `Set
      in
      match status with
      | `Empty ->
          Fmt.pr "Synced returned empty";
          Lwt.return_unit
      | `Head commit ->
          Fmt.pr "Synced: %a" Client.Commit.pp_hash commit;
          Lwt.return_unit)
  | Error e ->
      Fmt.pr "ERROR: %s\n" (Irmin_client.Error.to_string e);
      Lwt.return_unit

let () = Lwt_main.run main
