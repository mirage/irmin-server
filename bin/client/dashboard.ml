open Import
open Irmin_server
open Lwt.Infix
open Lwt.Syntax
module W = Nottui_widgets
module Ui = Nottui.Ui

let main (S ((module Client), client)) freq =
  let uptime = Lwd.var 0.0 in
  let branches = Lwd.var [] in
  let adds = Lwd.var 0 in
  let finds = Lwd.var 0 in
  let cache_misses = Lwd.var 0 in
  let ui =
    let open Lwd_infix in
    let$* uptime = Lwd.get uptime in
    let$* branches = Lwd.get branches in
    let$* adds = Lwd.get adds in
    let$* cache_misses = Lwd.get cache_misses in
    let$ finds = Lwd.get finds in
    let branches = List.map (W.printf "    %s") branches in
    Ui.keyboard_area
      (function `Escape, _ -> exit 0 | _, _ -> `Unhandled)
      (Ui.vcat
         ([
            W.printf "uptime: %.0fs" uptime;
            W.printf "adds: %d" adds;
            W.printf "finds: %d" finds;
            W.printf "cache_misses: %d" cache_misses;
            W.printf "branches:";
          ]
         @ branches))
  in
  let rec tick client () =
    Lwt.async (fun () ->
        let* stats = Client.stats client >|= Error.unwrap "stats" in
        Lwd.set uptime stats.uptime;
        Lwd.set branches stats.branches;
        Lwd.set adds stats.adds;
        Lwd.set finds stats.finds;
        Lwd.set cache_misses stats.cache_misses;
        let+ () = Lwt_unix.sleep freq in
        tick client ())
  in
  Lwt_main.run
    ( client >>= fun client ->
      tick client ();
      Nottui_lwt.run ui )
