open Lwt.Syntax

module Header = struct
  type t = { code : int }

  let v ~code = { code } [@@inline]
end

module Write = struct
  let header t Header.{ code; _ } =
    Logs.debug (fun l -> l "Writing response header: code=%d" code);
    let+ x = Lwt_io.LE.write_int t code in
    x
end

module Read = struct
  let header t =
    let+ code = Lwt_io.LE.read_int t in
    Logs.debug (fun l -> l "Read response header: n_items=%d" code);
    Header.{ code }
    [@@inline]

  let is_error Header.{ code; _ } = code < 0 [@@inline]

  let get_error t header =
    if is_error header then (
      let* x = Message.read t Irmin.Type.string in
      let x = Result.value ~default:"Unknown error" x in
      Logs.debug (fun l -> l "Error response message: %s" x);
      Lwt.return_some x)
    else Lwt.return_none
end
