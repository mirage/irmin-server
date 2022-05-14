let recode ?nln ?encoding out_encoding src dst =
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar u when u = Uutf.u_rep ->
        ignore (Uutf.encode e (`Uchar Uchar.min));
        loop d e
    | `Uchar _ as u ->
        ignore (Uutf.encode e u);
        loop d e
    | `End -> ignore (Uutf.encode e `End)
    | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep));
        loop d e
    | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding (`String src) in
  let e = Uutf.encoder out_encoding (`Buffer dst) in
  loop d e

let utf8_to_utf16 s =
  let b = Buffer.create (String.length s) in
  recode ~encoding:`UTF_8 `UTF_16 s b;
  Buffer.contents b

let utf16_to_utf8 s =
  let b = Buffer.create (String.length s) in
  recode ~encoding:`UTF_16 `UTF_8 s b;
  Buffer.contents b
