open Lwt.Syntax

module Header = struct
  type t = { n_items : int }

  let v ~n_items = { n_items }
end

module Write = struct
  let header t Header.{ n_items; _ } =
    Logs.debug (fun l -> l "Writing response header: n_items=%d" n_items);
    let+ x = Lwt_io.LE.write_int t n_items in
    x
end

module Read = struct
  let header t =
    let+ n_items = Lwt_io.LE.read_int t in
    Logs.debug (fun l -> l "Read response header: n_items=%d" n_items);
    Header.{ n_items }

  let is_error Header.{ n_items; _ } = n_items < 0

  let get_error t header =
    if is_error header then (
      let* x = Message.read t Irmin.Type.string in
      let x = Result.get_ok x in
      Logs.debug (fun l -> l "Error response message: %s" x);
      Lwt.return_some x)
    else Lwt.return_none
end
