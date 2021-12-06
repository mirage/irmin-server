open Import
open Lwt.Infix
open Lwt.Syntax
open Irmin_server_types
module W = Nottui_widgets
module Ui = Nottui.Ui

let handle_keyboard = function `Escape, _ -> exit 0 | _, _ -> `Unhandled

let bold = Notty.A.(st bold)

module Widget = struct
  type 'a t = { value : 'a Lwd.var; show : 'a -> Ui.t }

  let v show value =
    let value = Lwd.var value in
    { show; value }

  let title title fmt x = (W.printf ~attr:bold "%s: " title, W.printf fmt x)

  let display title' fmt value =
    let show x =
      let title, value = title title' fmt x in
      Ui.hcat [ title; value ]
    in
    v show value

  let get_value widget = Lwd.get widget.value

  let set_value widget x = Lwd.set widget.value x

  let show { show; value } =
    let open Lwd_infix in
    let$ x = Lwd.get value in
    show x
end

let uptime = Widget.display "Uptime" "%.0fs" 0.0

let pack =
  Widget.v
    (fun (size, adds, finds, cache_misses) ->
      Ui.vcat
        [
          W.printf ~attr:Notty.A.(st bold) "Pack:";
          W.printf "Size: %fM" size;
          W.printf "Adds: %d" adds;
          W.printf "Finds: %d" finds;
          W.printf "Cache misses: %f" cache_misses;
        ])
    (0., 0, 0, 0.)

let commit_diff (type a) (module Client : Irmin_client.S with type commit = a) x
    =
  let pr t a =
    let info = Client.Commit.info a in
    let date = Client.Info.date info in
    let localtime = Unix.localtime (Int64.to_float date) in
    W.printf "%s (%04d-%02d-%02d %02d:%02d:%02d)\ncommit: %s\ninfo: %s\n" t
      (localtime.tm_year + 1900) (localtime.tm_mon + 1) localtime.tm_mday
      localtime.tm_hour localtime.tm_min localtime.tm_sec
      (Irmin.Type.to_string Client.Commit.key_t (Client.Commit.key a))
      (Irmin.Type.to_string Client.Info.t info)
  in
  match x with
  | `Added a -> pr "Added" a
  | `Removed a -> pr "Removed" a
  | `Updated (_a, b) -> pr "Updated" b

let last_updates (type a) (module Client : Irmin_client.S with type commit = a)
    =
  Widget.v
    (fun last_updates ->
      let last_updates = List.map (commit_diff (module Client)) last_updates in
      Ui.vcat last_updates)
    []

let main client freq =
  client >>= fun (S ((module Client), client)) ->
  let last_updates = last_updates (module Client) in
  let ui =
    let open Lwd_infix in
    let$* uptime = Widget.show uptime in
    let$* pack = Widget.show pack in
    let$ last_updates = Widget.show last_updates in
    Ui.keyboard_area handle_keyboard
      (Ui.vcat
         [
           Ui.hcat
             [
               W.printf "Connected to %s" (Client.uri client |> Uri.to_string);
               Ui.space 5 1;
               uptime;
             ];
           Ui.space 0 1;
           pack;
           Ui.space 0 1;
           last_updates;
         ])
  in
  let rec tick client () =
    Lwt.async (fun () ->
        let* stats = Client.stats client >|= Error.unwrap "stats" in
        Widget.set_value uptime stats.uptime;
        Widget.set_value pack
          (stats.size, stats.adds, stats.finds.total, stats.cache_misses);
        let+ () = Lwt_unix.sleep freq in
        tick client ())
  in

  let watch client () =
    Lwt.async (fun () ->
        let f x =
          Widget.set_value last_updates (x :: Lwd.peek last_updates.value);
          Lwt.return_ok `Continue
        in
        Client.watch f client >|= Error.unwrap "watch")
  in

  let* wc = Client.dup client in
  watch wc ();
  tick client ();
  Nottui_lwt.run (W.scroll_area ui)

let main c f = main c f |> Lwt_main.run
