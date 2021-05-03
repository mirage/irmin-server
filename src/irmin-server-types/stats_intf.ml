type t = {
  uptime : float;
  branches : string list;
  finds : int;
  cache_misses : int;
  adds : int;
}
[@@deriving irmin]

module type Stats = sig
  type t = {
    uptime : float;
    branches : string list;
    finds : int;
    cache_misses : int;
    adds : int;
  }
  [@@deriving irmin]
end
