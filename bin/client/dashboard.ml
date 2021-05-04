open Import
open Lwt.Infix
open Lwt.Syntax
open Irmin_server_types
module W = Nottui_widgets
module Ui = Nottui.Ui

let handle_keyboard = function `Escape, _ -> exit 0 | _, _ -> `Unhandled

let main client freq =
  client >>= fun (S ((module Client), client)) ->
  let uptime = Lwd.var 0.0 in
  let adds = Lwd.var 0 in
  let finds = Lwd.var 0 in
  let cache_misses = Lwd.var 0 in
  let last_update = Lwd.var None in
  let ui =
    let open Lwd_infix in
    let$* uptime = Lwd.get uptime in
    let$* adds = Lwd.get adds in
    let$* last_update = Lwd.get last_update in
    let$* cache_misses = Lwd.get cache_misses in
    let$ finds = Lwd.get finds in
    let last_update =
      match last_update with
      | Some last_update ->
          Irmin.Type.to_json_string ~minify:false
            (Irmin.Diff.t Client.Commit.t)
            last_update
      | None -> "n/a"
    in
    Ui.keyboard_area handle_keyboard
      (Ui.vcat
         [
           W.printf "Connected to: %s" (Client.uri client |> Uri.to_string);
           W.printf "uptime: %.0fs" uptime;
           W.printf "adds: %d" adds;
           W.printf "finds: %d" finds;
           W.printf "cache_misses: %d" cache_misses;
           W.printf "last update: %s" last_update;
         ])
  in
  let rec tick client () =
    Lwt.async (fun () ->
        let* stats = Client.stats client >|= Error.unwrap "stats" in
        Lwd.set uptime stats.uptime;
        Lwd.set adds stats.adds;
        Lwd.set finds stats.finds;
        Lwd.set cache_misses stats.cache_misses;
        let+ () = Lwt_unix.sleep freq in
        tick client ())
  in

  let watch client () =
    Lwt.async (fun () ->
        let f x =
          Lwd.set last_update (Some x);
          Lwt.return_ok `Continue
        in
        Client.Store.watch f client >|= Error.unwrap "watch")
  in

  let* wc = Client.dup client in
  watch wc ();
  tick client ();
  Nottui_lwt.run ui

let main c f = main c f |> Lwt_main.run
