open Lwt.Syntax

module V1 = struct
  let s = "V1"

  let type_name x = Fmt.to_to_string Irmin.Type.pp_ty x

  let fingerprint (module Store : Irmin.S) : string =
    Irmin.Type.to_string Store.Hash.t Store.Tree.(hash empty)

  let send store ic oc =
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout 3.0 (fun () ->
            let* () = Lwt_io.write_line oc (s ^ fingerprint store) in
            let+ line = Lwt_io.read_line ic in
            assert (s = String.trim line)))
      (function
        | Assert_failure _ | Lwt_unix.Timeout ->
            Error.raise_error "unable to connect to server"
        | End_of_file -> Error.raise_error "invalid handshake"
        | x -> raise x)

  let check store ic oc =
    let* line = Lwt_unix.with_timeout 3.0 (fun () -> Lwt_io.read_line ic) in
    if String.trim line = s ^ fingerprint store then
      let* () = Lwt_io.write_line oc s in
      Lwt.return_true
    else Lwt.return_false
end
