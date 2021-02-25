open Lwt.Syntax
open Lwt.Infix

(* TODO: use encode_bin/decode_bin to prevent holding the entire value in memory at once *)

let write_raw oc s : unit Lwt.t =
  Lwt_io.LE.write_int oc (String.length s) >>= fun () -> Lwt_io.write oc s

let write oc t x : unit Lwt.t =
  let s = Irmin.Type.(unstage (to_bin_string t)) x in
  write_raw oc s

let read_raw ic =
  let* n = Lwt_io.LE.read_int ic in
  let buf = Bytes.create n in
  let+ () = Lwt_io.read_into_exactly ic buf 0 n in
  buf

let read ic t =
  let+ buf = read_raw ic in
  Irmin.Type.(unstage (of_bin_string t)) (Bytes.unsafe_to_string buf)
