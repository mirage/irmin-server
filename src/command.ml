include Command_intf

let of_name name =
  let s = String.lowercase_ascii name in
  Irmin.Type.of_string t ("\"" ^ s ^ "\"")

let name x =
  let s = Irmin.Type.to_string t x in
  String.sub s 1 (String.length s - 2) |> String.lowercase_ascii
