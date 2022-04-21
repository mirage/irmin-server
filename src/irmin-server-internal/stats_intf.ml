type t = {
  uptime : float;
  branches : (string * string option) list;
  cache_misses : float;
  adds : int;
}
[@@deriving irmin]

module type Stats = sig
  type nonrec t = t = {
    uptime : float;
    branches : (string * string option) list;
    cache_misses : float;
    adds : int;
  }
  [@@deriving irmin]
end
