type flow = Conduit_lwt_unix.flow
type ic = Conduit_lwt_unix.ic
type oc = Conduit_lwt_unix.oc
type ctx = Conduit_lwt_unix.ctx

exception Timeout = Lwt_unix.Timeout

let default_ctx = Conduit_lwt_unix.default_ctx
let is_closed (x : ic) = Lwt_io.is_closed x
let write_int64_be = Lwt_io.BE.write_int64
let read_int64_be = Lwt_io.BE.read_int64
let flush = Lwt_io.flush
let write = Lwt_io.write
let read_into_exactly = Lwt_io.read_into_exactly
let write_char = Lwt_io.write_char
let read_char = Lwt_io.read_char

let websocket_to_flow client =
  let open Lwt.Infix in
  let flow = Obj.magic () in (* BAD! *)
  let rec fill_ic channel client =
    Websocket_lwt_unix.read client >>= fun frame ->
    Lwt_io.write channel frame.content >>= fun () ->
    fill_ic channel client
  in
  let rec send_oc channel client =
    Lwt_io.read channel >>= fun content ->
    Logs.debug (fun f -> f "Client send: %s%!" content);
    Websocket_lwt_unix.write client (Websocket.Frame.create ~content ()) >>= fun () ->
    send_oc channel client
  in
  let input_ic, input_oc = Lwt_io.pipe () in
  let output_ic, output_oc = Lwt_io.pipe () in
  Lwt.async (fun () -> fill_ic input_oc client);
  Lwt.async (fun () -> send_oc output_ic client);
  flow, input_ic, output_oc

let connect ~ctx (client : Irmin_client.addr) = match client with
  | `TLS _ | `TCP _
  | `Unix_domain_socket _ as client -> Conduit_lwt_unix.connect ~ctx (client :> Conduit_lwt_unix.client)
  | `Ws (host, port, uri) ->
    let open Lwt.Infix in 
    Websocket_lwt_unix.connect ~ctx (`TCP (host, port)) (Uri.of_string uri) >|= fun ws ->
    websocket_to_flow ws

let close (c : ic * oc) = Conduit_lwt_server.close c
let with_timeout = Lwt_unix.with_timeout
let time = Unix.time
