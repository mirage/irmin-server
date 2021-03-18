open Lwt.Syntax

type 'a t = { code : int; conn : Conn.t }

let make code conn : 'a t Lwt.t =
  let x = { code; conn } in
  let+ () = Response.Write.header conn.oc Response.Header.{ code } in
  x
  [@@inline]

let ok conn : unit t Lwt.t = make 0 conn [@@inline]

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

(*let check t ~n_results:c =
  let x = if c < 0 then t.n_items <= abs c else t.n_items = c in
  assert ((x && t.index = t.n_items) || t.n_items = -1)
  [@@inline]*)

let flush t = Lwt_io.flush t.conn.oc [@@inline]
