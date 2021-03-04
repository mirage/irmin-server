open Lwt.Syntax

module V1 = struct
  type t = [ `V1 ] [@@deriving irmin]

  let send ic oc =
    Lwt.catch
      (fun () ->
        let s = Irmin.Type.to_string t `V1 in
        Lwt_unix.with_timeout 3.0 (fun () ->
            let* () = Lwt_io.write_line oc s in
            let+ s = Lwt_io.read_line ic in
            let x =
              match Irmin.Type.of_string t (String.trim s) with
              | Ok `V1 -> true
              | Error _ -> false
            in
            assert x))
      (function
        | Assert_failure _ | Lwt_unix.Timeout ->
            Error.raise_error 0 "unable to connect to server"
        | x -> raise x)

  let check ic oc =
    let* s = Lwt_unix.with_timeout 3.0 (fun () -> Lwt_io.read_line ic) in
    match Irmin.Type.of_string t (String.trim s) with
    | Ok `V1 ->
        let* () = Lwt_io.write_line oc s in
        Lwt.return_true
    | Error _ -> Lwt.return_false
end
