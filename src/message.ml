open Lwt.Syntax
open Lwt.Infix

let decode t = Irmin.Type.(unstage (of_bin_string t)) [@@inline]

let encode t = Irmin.Type.(unstage (to_bin_string t)) [@@inline]

let write_raw oc s : unit Lwt.t =
  let len = String.length s in
  Logs.debug (fun l -> l "Writing raw message: length=%d" len);
  let* x =
    Lwt_io.BE.write_int64 oc (Int64.of_int len) >>= fun () ->
    if len = 0 then Lwt.return_unit else Lwt_io.write oc s
  in
  let+ () = Lwt_io.flush oc in
  x

let write oc t x : unit Lwt.t =
  let s = encode t x in
  write_raw oc s
  [@@inline]

let read_raw buffer ic =
  let* n = Lwt_io.BE.read_int64 ic in
  Logs.debug (fun l -> l "Raw message length=%Ld" n);
  if n <= 0L then Lwt.return Bytes.empty
  else
    let n = Int64.to_int n in
    let buf =
      if n > Bytes.length buffer then Bytes.create n else Bytes.sub buffer 0 n
    in
    let+ () = Lwt_io.read_into_exactly ic buf 0 n in
    buf

let read buffer ic t =
  let+ buf = read_raw buffer ic in
  decode t (Bytes.unsafe_to_string buf)
  [@@inline]
