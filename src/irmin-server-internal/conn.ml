open Lwt.Syntax
open Lwt.Infix
include Conn_intf

module Codec = struct
  module type S = Codec.S

  module Bin = struct
    let decode t = Irmin.Type.(unstage (of_bin_string t)) [@@inline]
    let encode t = Irmin.Type.(unstage (to_bin_string t)) [@@inline]
  end

  module Json = struct
    let decode t = Irmin.Type.of_json_string t [@@inline]
    let encode t = Irmin.Type.to_json_string t [@@inline]
  end
end

module Make (I : IO) (T : Codec.S) = struct
  module IO = I

  type t = { flow : IO.flow; ic : IO.ic; oc : IO.oc; buffer : bytes }

  let v ?(buffer_size = 4096) flow ic oc =
    { flow; ic; oc; buffer = Bytes.create buffer_size }
    [@@inline]

  let is_closed { ic; _ } = IO.is_closed ic

  let write_raw t s : unit Lwt.t =
    let len = String.length s in
    Logs.debug (fun l -> l "Writing raw message: length=%d" len);
    let* x =
      IO.write_int64_be t.oc (Int64.of_int len) >>= fun () ->
      if len = 0 then Lwt.return_unit else IO.write t.oc s
    in
    let+ () = IO.flush t.oc in
    x

  let write t ty x : unit Lwt.t =
    let s = T.encode ty x in
    write_raw t s

  let read_raw ~buffer t =
    let* n = IO.read_int64_be t.ic in
    Logs.debug (fun l -> l "Raw message length=%Ld" n);
    if n <= 0L then Lwt.return Bytes.empty
    else
      let n = Int64.to_int n in
      let buf =
        if n > Bytes.length buffer then Bytes.create n else Bytes.sub buffer 0 n
      in
      let+ () = IO.read_into_exactly t.ic buf 0 n in
      buf

  let read ~buffer t ty =
    let+ buf = read_raw ~buffer t in
    T.decode ty (Bytes.unsafe_to_string buf)
    [@@inline]

  module Handshake = struct
    module V1 = struct
      let version = "V1"

      let fingerprint (module Store : Irmin.Generic_key.S) : string =
        let hex = Irmin.Type.to_string Store.Hash.t in
        let hash = Store.Hash.hash (fun f -> f "V1") in
        hex hash

      let send store t =
        Lwt.catch
          (fun () ->
            Lwt_unix.with_timeout 3.0 (fun () ->
                let s = fingerprint store in
                let* () = IO.write_line t.oc s in
                let+ line = IO.read_line t.ic in
                assert (s = String.trim line)))
          (function
            | Assert_failure _ | Lwt_unix.Timeout ->
                Error.raise_error "unable to connect to server"
            | End_of_file -> Error.raise_error "invalid handshake"
            | x -> raise x)

      let check store t =
        let s = fingerprint store in
        let* line = Lwt_unix.with_timeout 3.0 (fun () -> IO.read_line t.ic) in
        if String.trim line = s then
          let* () = IO.write_line t.oc s in
          Lwt.return_true
        else Lwt.return_false
    end
  end

  module Response = struct
    type header = { status : int }

    let v_header ~status = { status } [@@inline]

    let write_header t { status; _ } =
      Logs.debug (fun l -> l "Writing response header: status=%d" status);
      let+ x = IO.write_char t.oc (char_of_int status) in
      x

    let read_header t =
      Logs.debug (fun l -> l "Starting response header read");
      let+ status = IO.read_char t.ic in
      let status = int_of_char status in
      Logs.debug (fun l -> l "Read response header: status=%d" status);
      { status }
      [@@inline]

    let is_error { status; _ } = status >= 1 [@@inline]

    let get_error buffer t header =
      if is_error header then (
        let* x = read ~buffer t Irmin.Type.string in
        let x = Result.value ~default:"Unknown error" x in
        Logs.debug (fun l -> l "Error response message: %s" x);
        Lwt.return_some x)
      else Lwt.return_none
  end

  module Request = struct
    type header = { command : string }

    let v_header ~command = { command } [@@inline]

    let write_header t { command } : unit Lwt.t =
      Logs.debug (fun l -> l "Writing request header: command=%s" command);
      IO.write_line t.oc (String.lowercase_ascii command)

    let read_header t : header Lwt.t =
      let+ command = IO.read_line t.ic >|= String.trim in
      let command = String.lowercase_ascii command in
      Logs.debug (fun l -> l "Request header read: command=%s" command);
      { command }
  end

  module Return = struct
    type conn = t
    type 'a t = { status : int; conn : conn }

    let make status conn : 'a t Lwt.t =
      let x = { status; conn } in
      let+ () = Response.write_header conn Response.{ status } in
      x
      [@@inline]

    let err conn msg : 'a t Lwt.t =
      let* t = make 1 conn in
      let+ () = write conn Irmin.Type.string ("ERROR " ^ msg) in
      t
      [@@inline]

    let write ty x t =
      let+ () = write t.conn ty x in
      t
      [@@inline]

    let v client ty (x : 'a) : 'a t Lwt.t =
      let* r = make 0 client in
      write ty x r
      [@@inline]

    let ok conn : unit t Lwt.t = v conn Irmin.Type.unit () [@@inline]

    let result conn t x =
      match x with Ok x -> v conn t x | Error (`Msg msg) -> err conn msg

    let finish _t = Lwt.pause ()
  end

  let begin_response t n = Response.write_header t { status = n } [@@inline]
  let ok t = begin_response t 1 [@@inline]

  let err t msg =
    let header = Response.v_header ~status:1 in
    let msg = "ERROR " ^ msg in
    let* () = Response.write_header t header in
    write t Irmin.Type.string msg
end
