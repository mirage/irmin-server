open Lwt.Syntax

module V1 = struct
  let type_name x = Fmt.to_to_string Irmin.Type.pp_ty x

  let fingerprint (module Store : Irmin.S) : string =
    let hex = Irmin.Type.to_string Store.Hash.t in
    let contents_name = type_name Store.Contents.t in
    let hash = Store.Hash.hash (fun f -> f contents_name) in
    hex hash

  let send store ic oc =
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout 3.0 (fun () ->
            let s = fingerprint store in
            let* () = Lwt_io.write_line oc s in
            let+ line = Lwt_io.read_line ic in
            assert (s = String.trim line)))
      (function
        | Assert_failure _ | Lwt_unix.Timeout ->
            Error.raise_error "unable to connect to server"
        | End_of_file -> Error.raise_error "invalid handshake"
        | x -> raise x)

  let check store ic oc =
    let s = fingerprint store in
    let* line = Lwt_unix.with_timeout 3.0 (fun () -> Lwt_io.read_line ic) in
    if String.trim line = s then
      let* () = Lwt_io.write_line oc s in
      Lwt.return_true
    else Lwt.return_false
end
