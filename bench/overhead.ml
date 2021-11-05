module Key = Irmin.Path.String_list

let encode_path = Irmin.Type.(unstage (to_bin_string Key.t))

let decode_path = Irmin.Type.(unstage (of_bin_string Key.t))

let random_path () =
  let steps = 2 + Random.int 14 in
  let rec aux steps acc =
    if steps = 0 then acc
    else aux (steps - 1) (Bench_common.random_path () :: acc)
  in
  aux steps []

let check_encoding_path path =
  let enc = encode_path path in
  let dec = decode_path enc in
  let dec = Result.get_ok dec in
  assert (List.length dec > 0)

let check_direct_path path = assert (List.length path > 0)

open Benchmark

let () =
  let res =
    latencyN ~repeat:1000 5000L
      [
        ("path encoding", check_encoding_path, random_path ());
        ("direct", check_direct_path, random_path ());
      ]
  in
  print_endline "Key encoding latency";
  tabulate res
