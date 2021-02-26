open Lwt.Syntax
open Lwt.Infix

module Header = struct
  type t = { command : Command.t; n_args : int }

  let v ~n_args ~command = { n_args; command }
end

module Write = struct
  type t = Conduit_lwt_unix.oc

  let header t Header.{ command; n_args } : unit Lwt.t =
    Logs.debug (fun l ->
        l "Writing request header: command=%s, n_args=%d" (Command.name command)
          n_args);
    let s = Command.name command in
    let* () = Lwt_io.write_line t s in
    Lwt_io.LE.write_int t n_args
end

module Read = struct
  type t = Conduit_lwt_unix.ic

  let header t : Header.t Lwt.t =
    let* command = Lwt_io.read_line t >|= String.trim in
    let command = Command.of_name command in
    let+ n_args = Lwt_io.LE.read_int t in
    Logs.debug (fun l ->
        l "Request header read: command=%s, n_args=%d" (Command.name command)
          n_args);
    Header.{ command; n_args }
end
