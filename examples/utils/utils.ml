let default_uri = Uri.of_string "tcp://localhost:9090"

let get_url =
  try Uri.of_string Sys.argv.(1) with Invalid_argument _ -> default_uri

let server_uri = Uri.of_string "ws://localhost:9090/ws"
