let ( let* ) x f = Lwt.bind x f

let ( let+ ) x f = Lwt.map f x

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Unix.gettimeofday () in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let () =
  Logs.set_level (Some Logs.App);
  Logs.set_reporter (reporter ())

let reset_stats () =
  Index.Stats.reset_stats ();
  Irmin_pack.Stats.reset_stats ()

let random_char () = char_of_int (Random.int 255)

let random_string n = String.init n (fun _i -> random_char ())

let random_blob () = random_string 10

let random_key () = random_string 5

let with_timer f =
  let t0 = Sys.time () in
  let+ a = f () in
  let t1 = Sys.time () -. t0 in
  (t1, a)

module Conf = struct
  let entries = 32

  let stable_hash = 256
end
