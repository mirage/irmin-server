open Lwt.Syntax
open Lwt.Infix

module Header = struct
  type t = { command : string }

  let v ~command = { command } [@@inline]
end

module Write = struct
  type t = Conduit_lwt_unix.oc

  let header t Header.{ command } : unit Lwt.t =
    Logs.debug (fun l -> l "Writing request header: command=%s" command);
    Lwt_io.write_line t (String.lowercase_ascii command)
end

module Read = struct
  type t = Conduit_lwt_unix.ic

  let header t : Header.t Lwt.t =
    let+ command = Lwt_io.read_line t >|= String.trim in
    let command = String.lowercase_ascii command in
    Logs.debug (fun l -> l "Request header read: command=%s" command);
    Header.{ command }
end
