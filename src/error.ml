include Error_intf

let raise_error n msg = raise (Error (n, msg))

let unwrap = function Ok x -> x | Error (`Msg e) -> raise (Unwrap e)

let () =
  Printexc.register_printer (function
    | Error (_, msg) -> Some msg
    | Unwrap msg -> Some msg
    | exn -> raise exn)
