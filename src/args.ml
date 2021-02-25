open Lwt.Syntax
open Error

type t = { last_index : int; mutable index : int; conn : Conn.t }

let v ~count conn = { last_index = count - 1; index = 0; conn }

let next_raw t =
  if t.index >= t.last_index then raise_error 0 "ERROR expected argument"
  else
    let+ x = Item.read_raw t.conn.ic in
    t.index <- t.index + 1;
    x

let next t ty =
  if t.index >= t.last_index then raise_error 0 "ERROR expected argument"
  else
    let+ x = Conn.read_arg t.conn ty in
    t.index <- t.index + 1;
    x

let remaining t = t.last_index - t.index

let raise_error t msg = raise_error (remaining t) msg
