let get_url =
  try Uri.of_string Sys.argv.(1)
  with Invalid_argument _ -> Uri.of_string "tcp://localhost:9090"

let server_uri = Uri.of_string "ws://localhost:9090/ws"
