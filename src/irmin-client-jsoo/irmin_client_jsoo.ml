open Lwt.Infix
open Irmin_server_internal
module Error = Error

module Info (I : Irmin.Info.S) = struct
  include I

  let init = v

  let v ?author fmt =
    Fmt.kstr
      (fun message () ->
        let date = Int64.of_float (Unix.gettimeofday ()) in
        init ?author ~message date)
      fmt
end

module type S = sig
  include Irmin_client.S

end

module IO = struct
  open Brr_io

  type flow = Websocket.t
  type ic = flow * string Lwt_stream.t * (string option -> unit)
  type oc = flow * string Lwt_stream.t * (string option -> unit)

  exception Timeout

  let is_closed (ws, _, _) = 
    Websocket.ready_state ws = 3

  let write (_, _, push) s = 
    Lwt.return @@ push (Some s)

  let send (ws, _, _) s = 
    Websocket.send_string ws (Jstr.v s);
    Lwt.return ()

  let read (_, s, _) =
    Lwt_stream.next s

  let write_int64_be _ _ = failwith "TODO"
  let read_int64_be _ = failwith "TODO"
  let flush oc = 
    let rec aux ((_, s, _) as oc) = 
      Lwt_stream.is_empty s >>= fun b ->
      if b then Lwt.return () else
      Lwt_stream.next s >>= fun t ->
      send oc t >>= fun () ->
      aux oc
    in
      aux oc

  let write_line oc s = write oc s

  let read_line ic = read ic

  let read_into_exactly _ _ _ _ = failwith "TODO"

  let write_char oc c = 
    write oc (String.make 1 c)

  let read_char ic = 
    read ic >|= fun s -> s.[0]

  let with_timeout _ _ = failwith "TODO"

  let time () = failwith "TODO"

  type ctx = unit

  let default_ctx = lazy ()

  let connect ~ctx:_ (client : Irmin_client.addr) =
    match client with
    | `Ws (_, _, s) -> 
      let ws = Websocket.create @@ Jstr.v s in
      let (is, s) = Lwt_stream.create () in
      let (ir, r) = Lwt_stream.create () in
      Lwt.return (ws, (ws, is, s), (ws, ir, r))
    | `TLS _ | `TCP _
    | `Unix_domain_socket _ -> failwith "Unsupported Protocol"

  let close (((c, _, _),_) : ic * oc) = Lwt.return @@ Websocket.close c
end

module Make
  (Codec : Irmin_server_internal.Conn.Codec.S)
  (Store : Irmin.Generic_key.S) = struct
  include Irmin_client.Make_ext (IO) (Codec) (Store)
end