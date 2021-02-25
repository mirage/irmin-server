include Error_intf

let raise_error n msg = raise (Error (n, msg))

let () =
  Printexc.register_printer (function
    | Error (_, msg) -> Some msg
    | exn -> raise exn)
