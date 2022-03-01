type flow = Conduit_lwt_unix.flow
type ic = Conduit_lwt_unix.ic
type oc = Conduit_lwt_unix.oc
type ctx = Conduit_lwt_unix.ctx

let default_ctx = Conduit_lwt_unix.default_ctx
let is_closed (x : ic) = Lwt_io.is_closed x
let write_int64_be = Lwt_io.BE.write_int64
let read_int64_be = Lwt_io.BE.read_int64
let flush = Lwt_io.flush
let write = Lwt_io.write
let read_into_exactly = Lwt_io.read_into_exactly
let write_line = Lwt_io.write_line
let read_line = Lwt_io.read_line
let write_char = Lwt_io.write_char
let read_char = Lwt_io.read_char

let connect ~ctx (client : Irmin_client.addr) =
  Conduit_lwt_unix.connect ~ctx (client :> Conduit_lwt_unix.client)

let close (c : ic * oc) = Conduit_lwt_server.close c
