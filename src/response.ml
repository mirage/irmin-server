open Lwt.Syntax

module Header = struct
  type t = { n_items : int }

  let v ~n_items = { n_items }
end

module Write = struct
  let header t Header.{ n_items; _ } = Lwt_io.write_int t n_items
end

module Read = struct
  let header t =
    let+ n_items = Lwt_io.read_int t in
    Header.{ n_items }

  let is_error Header.{ n_items; _ } = n_items < 0

  let get_error t header =
    if is_error header then
      let* x = Item.read t Irmin.Type.string in
      match x with Ok x -> Lwt.return_some x | Error _ -> Lwt.return_none
    else Lwt.return_none
end
