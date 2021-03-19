open Lwt.Syntax

module V1 = struct
  let s = "V1"

  let send ic oc =
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout 3.0 (fun () ->
            let* () = Lwt_io.write_line oc s in
            let+ line = Lwt_io.read_line ic in
            assert (s = String.trim line)))
      (function
        | Assert_failure _ | Lwt_unix.Timeout ->
            Error.raise_error "unable to connect to server"
        | x -> raise x)

  let check ic oc =
    let* line = Lwt_unix.with_timeout 3.0 (fun () -> Lwt_io.read_line ic) in
    if String.trim line = s then
      let* () = Lwt_io.write_line oc s in
      Lwt.return_true
    else Lwt.return_false
end
