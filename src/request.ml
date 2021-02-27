open Lwt.Syntax
open Lwt.Infix

module Header = struct
  type t = { command : string; n_args : int }

  let v ~n_args ~command = { n_args; command } [@@inline]
end

module Write = struct
  type t = Conduit_lwt_unix.oc

  let header t Header.{ command; n_args } : unit Lwt.t =
    Logs.debug (fun l ->
        l "Writing request header: command=%s, n_args=%d" command n_args);
    let* () = Lwt_io.write_line t (String.lowercase_ascii command) in
    Lwt_io.LE.write_int t n_args
end

module Read = struct
  type t = Conduit_lwt_unix.ic

  let header t : Header.t Lwt.t =
    let* command = Lwt_io.read_line t >|= String.trim in
    let command = String.lowercase_ascii command in
    let+ n_args = Lwt_io.LE.read_int t in
    Logs.debug (fun l ->
        l "Request header read: command=%s, n_args=%d" command n_args);
    Header.{ command; n_args }
end
