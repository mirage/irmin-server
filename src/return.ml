open Lwt.Syntax

type 'a t = { status : int; conn : Conn.t }

let make status conn : 'a t Lwt.t =
  let x = { status; conn } in
  let+ () = Response.Write.header conn.oc Response.Header.{ status } in
  x
  [@@inline]

let err conn msg : 'a t Lwt.t =
  let* t = make 1 conn in
  let+ () = Conn.write_message conn Irmin.Type.string ("ERROR " ^ msg) in
  t
  [@@inline]

let write ty x t =
  let+ () = Conn.write_message t.conn ty x in
  t
  [@@inline]

let v client ty (x : 'a) : 'a t Lwt.t =
  let* r = make 0 client in
  write ty x r
  [@@inline]

let ok conn : unit t Lwt.t = v conn Irmin.Type.unit () [@@inline]

let result conn t x =
  match x with Ok x -> v conn t x | Error (`Msg msg) -> err conn msg

let finish _ty _t = Lwt_unix.yield ()
