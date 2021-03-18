open Lwt.Syntax

type 'a t = { status : int; conn : Conn.t }

let make status conn : 'a t Lwt.t =
  let x = { status; conn } in
  let+ () = Response.Write.header conn.oc Response.Header.{ status } in
  x
  [@@inline]

let err conn msg : 'a t Lwt.t =
  let* t = make (-1) conn in
  let+ () = Message.write conn.oc Irmin.Type.string ("ERROR " ^ msg) in
  t
  [@@inline]

let write ty x t =
  let+ () = Message.write t.conn.oc ty x in
  t
  [@@inline]

let v client ty (x : 'a) : 'a t Lwt.t =
  let* r = make 1 client in
  write ty x r
  [@@inline]

let ok conn : unit t Lwt.t = v conn Irmin.Type.unit () [@@inline]

(*let check t ~n_results:c =
  let x = if c < 0 then t.n_items <= abs c else t.n_items = c in
  assert ((x && t.index = t.n_items) || t.n_items = -1)
  [@@inline]*)

let flush t = Lwt_io.flush t.conn.oc [@@inline]
