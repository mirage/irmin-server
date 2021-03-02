open Error
open Lwt.Syntax

type 'a t = { last_index : int; mutable index : int; conn : Conn.t; mode : 'a }

let v ~mode ~count conn =
  { last_index = count; index = 0; conn; mode }
  [@@inline]

let next_raw t =
  if t.index > t.last_index then
    raise_error 0 "expected argument in Message.next_raw"
  else
    let+ x = Message.read_raw t.conn.ic in
    t.index <- t.index + 1;
    x
  [@@inline]

let next t ty =
  if t.index > t.last_index then
    raise_error 0 "expected argument in Message.next"
  else
    let+ x = Conn.read_message t.conn ty in
    t.index <- t.index + 1;
    x
  [@@inline]

let write t ty x =
  if t.index > t.last_index then
    raise_error 0 "too many arguments in Message.write"
  else
    let+ x = Conn.write_message t.conn ty x in
    t.index <- t.index + 1;
    x
  [@@inline]

let remaining t = t.last_index - t.index [@@inline]

let raise_error t msg = raise_error (remaining t) msg
