open Lwt.Syntax

type t = {
  flow : Conduit_lwt_unix.flow;
  ic : Conduit_lwt_unix.ic;
  oc : Conduit_lwt_unix.oc;
  buffer : bytes;
}

let v flow ic oc = { flow; ic; oc; buffer = Bytes.create 4096 } [@@inline]

let read_message t ty = Message.read t.buffer t.ic ty [@@inline]

let write_message t ty x = Message.write t.oc ty x [@@inline]

let begin_response t n = Response.Write.header t.oc { status = n } [@@inline]

let ok t = begin_response t 1 [@@inline]

let err t msg =
  let header = Response.Header.v ~status:1 in
  let msg = "ERROR " ^ msg in
  let* () = Response.Write.header t.oc header in
  Message.write t.oc Irmin.Type.string msg
