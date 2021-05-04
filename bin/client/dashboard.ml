open Import
open Lwt.Infix
open Lwt.Syntax
open Irmin_server_types
module W = Nottui_widgets
module Ui = Nottui.Ui

let handle_keyboard = function `Escape, _ -> exit 0 | _, _ -> `Unhandled

let bold = Notty.A.(st bold)

let magenta = Notty.A.(fg magenta)

module Widget = struct
  type 'a t = { value : 'a Lwd.var; show : 'a -> Ui.t }

  let v show value =
    let value = Lwd.var value in
    { show; value }

  let show_title title fmt x = (W.printf ~attr:bold "%s: " title, W.printf fmt x)

  let display title fmt value =
    let show x =
      let title, value = show_title title fmt x in
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
    (fun (adds, finds, cache_misses) ->
      Ui.vcat
        [
          W.printf ~attr:Notty.A.(st bold) "Pack:";
          W.printf "Adds: %d" adds;
          W.printf "Finds: %d" finds;
          W.printf "Cache misses: %d" cache_misses;
        ])
    (0, 0, 0)

let last_update (type a) (module Client : Irmin_client.S with type commit = a) =
  Widget.v
    (fun last_update ->
      let last_update =
        match last_update with
        | Some last_update ->
            Irmin.Type.to_json_string ~minify:false
              (Irmin.Diff.t Client.Commit.t)
              last_update
        | None -> "n/a"
      in
      let title, value = Widget.show_title "Last update" "%s" last_update in
      Ui.hcat [ title; value ])
    None

let main client freq =
  client >>= fun (S ((module Client), client)) ->
  let last_update = last_update (module Client) in
  let ui =
    let open Lwd_infix in
    let$* uptime = Widget.show uptime in
    let$* pack = Widget.show pack in
    let$ last_update = Widget.show last_update in
    Ui.keyboard_area handle_keyboard
      (Ui.vcat
         [
           W.printf ~attr:magenta "Connected to: %s"
             (Client.uri client |> Uri.to_string);
           Ui.space 0 1;
           uptime;
           Ui.space 0 1;
           pack;
           Ui.space 0 1;
           last_update;
         ])
  in
  let rec tick client () =
    Lwt.async (fun () ->
        let* stats = Client.stats client >|= Error.unwrap "stats" in
        Widget.set_value uptime stats.uptime;
        Widget.set_value pack (stats.adds, stats.finds, stats.cache_misses);
        let+ () = Lwt_unix.sleep freq in
        tick client ())
  in

  let watch client () =
    Lwt.async (fun () ->
        let f x =
          Widget.set_value last_update (Some x);
          Lwt.return_ok `Continue
        in
        Client.Store.watch f client >|= Error.unwrap "watch")
  in

  let* wc = Client.dup client in
  watch wc ();
  tick client ();
  Nottui_lwt.run ui

let main c f = main c f |> Lwt_main.run
