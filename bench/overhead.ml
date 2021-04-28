module Key = Irmin.Path.String_list

let encode_key = Irmin.Type.(unstage (to_bin_string Key.t))

let decode_key = Irmin.Type.(unstage (of_bin_string Key.t))

let random_key () =
  let steps = 2 + Random.int 14 in
  let rec aux steps acc =
    if steps = 0 then acc
    else aux (steps - 1) (Bench_common.random_key () :: acc)
  in
  aux steps []

let check_encoding_key key =
  let enc = encode_key key in
  let dec = decode_key enc in
  let dec = Result.get_ok dec in
  assert (List.length dec > 0)

let check_direct_key key = assert (List.length key > 0)

open Benchmark

let () =
  let res =
    latencyN ~repeat:1000 5000L
      [
        ("key encoding", check_encoding_key, random_key ());
        ("direct", check_direct_key, random_key ());
      ]
  in
  print_endline "Key encoding latency";
  tabulate res
