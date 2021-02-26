open Lwt.Syntax
open Lwt.Infix

(* TODO: use encode_bin/decode_bin to prevent holding the entire value in memory at once *)

let decode t = Irmin.Type.(unstage (of_bin_string t)) [@@inline]

let encode t = Irmin.Type.(unstage (to_bin_string t)) [@@inline]

let write_raw oc s : unit Lwt.t =
  let len = String.length s in
  Logs.debug (fun l -> l "Writing raw message: length=%d" len);
  let+ x = Lwt_io.LE.write_int oc len >>= fun () -> Lwt_io.write oc s in
  x

let write oc t x : unit Lwt.t =
  let s = encode t x in
  write_raw oc s
  [@@inline]

let read_raw ic =
  let* n = Lwt_io.LE.read_int ic in
  Logs.debug (fun l -> l "Raw message length=%d" n);
  let buf = Bytes.create n in
  let+ () = Lwt_io.read_into_exactly ic buf 0 n in
  buf

let read ic t =
  let+ buf = read_raw ic in
  decode t (Bytes.to_string buf)
  [@@inline]
