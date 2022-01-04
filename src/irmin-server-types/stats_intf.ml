type t = {
  uptime : float;
  branches : (string * string option) list;
  finds : Irmin_pack.Stats.Find.t;
  cache_misses : float;
  adds : int;
  size : float;
}
[@@deriving irmin]

module type Stats = sig
  type t = {
    uptime : float;
    branches : (string * string option) list;
    finds : Irmin_pack.Stats.Find.t;
    cache_misses : float;
    adds : int;
    size : float;
  }
  [@@deriving irmin]
end
