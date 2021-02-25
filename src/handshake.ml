open Lwt.Syntax

module V1 = struct
  type t = [ `V1 ] [@@deriving irmin]

  let send oc =
    let s = Irmin.Type.to_string t `V1 in
    Lwt_io.write_line oc s

  let check ic =
    let* s = Lwt_io.read_line ic in
    match Irmin.Type.of_string t (String.trim s) with
    | Ok `V1 -> Lwt.return_true
    | Error _ -> Lwt.return_false
end
