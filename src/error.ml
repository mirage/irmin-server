include Error_intf

let raise_error n msg = raise (Error (n, msg))

let unwrap prefix = function
  | Ok x -> x
  | Error (`Msg e) -> raise (Unwrap (prefix ^ ": " ^ e))

let of_string s = `Msg s

let to_string = function `Msg s -> s

let () =
  Printexc.register_printer (function
    | Error (_, msg) -> Some msg
    | Unwrap msg -> Some msg
    | exn -> raise exn)
