let get_url =
  try Uri.of_string Sys.argv.(1)
  with Invalid_argument _ -> Uri.of_string "tcp://localhost:9090"
