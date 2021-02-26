open Lwt.Syntax

type t = {
  flow : Conduit_lwt_unix.flow;
  ic : Conduit_lwt_unix.ic;
  oc : Conduit_lwt_unix.oc;
}

let v flow ic oc = { flow; ic; oc }

let read_arg t ty = Message.read t.ic ty

let write_arg t ty x = Message.write t.oc ty x

let begin_response t n = Response.Write.header t.oc { n_items = n }

let ok t = begin_response t 0

let err t msg =
  let header = Response.Header.v ~n_items:(-1) in
  let* () = Response.Write.header t.oc header in
  Message.write t.oc Irmin.Type.string msg

let consume t n =
  let rec aux t n =
    if n = 0 then Lwt.return_unit
    else
      let* _ = Message.read_raw t.ic in
      aux t (n - 1)
  in
  aux t n
