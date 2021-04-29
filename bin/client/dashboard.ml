open Import
open Irmin_server
open Lwt.Infix
open Lwt.Syntax
module W = Nottui_widgets
module Ui = Nottui.Ui

let main (S ((module Client), client)) =
  let uptime = Lwd.var 0.0 in
  let head = Lwd.var None in
  let ui =
    let open Lwd_infix in
    let$* uptime = Lwd.get uptime in
    let$ head = Lwd.get head in
    let head =
      Option.map (Irmin.Type.to_string Client.Hash.t) head
      |> Option.value ~default:"none"
    in
    Ui.keyboard_area
      (function `Escape, _ -> exit 0 | _, _ -> `Unhandled)
      (Ui.vcat [ W.printf "uptime: %.0fs" uptime; W.printf "head: %s" head ])
  in
  let rec tick client () =
    Lwt.async (fun () ->
        let* stats = Client.stats client >|= Error.unwrap "stats" in
        Lwd.set uptime stats.uptime;
        Lwd.set head stats.head;
        let+ () = Lwt_unix.sleep 1.0 in
        tick client ())
  in
  Lwt_main.run
    ( client >>= fun client ->
      tick client ();
      Nottui_lwt.run ui )
