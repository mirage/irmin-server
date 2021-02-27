open Lwt.Syntax

type t = { n_items : int; mutable index : int; conn : Conn.t }

let make n_items conn =
  let x = { n_items; index = 0; conn } in
  let+ () = Response.Write.header conn.oc Response.Header.{ n_items } in
  x
  [@@inline]

let ok conn = make 0 conn [@@inline]

let err conn msg =
  let* t = make (-1) conn in
  let+ () = Message.write conn.oc Irmin.Type.string msg in
  t
  [@@inline]

let write ty x t =
  let+ () = Message.write t.conn.oc ty x in
  t.index <- t.index + 1;
  t
  [@@inline]

let v client ty x =
  let* r = make 1 client in
  write ty x r
  [@@inline]

let check t c =
  assert ((t.n_items = c && t.index = t.n_items) || t.n_items = -1)
  [@@inline]

let flush t = Lwt_io.flush t.conn.oc
