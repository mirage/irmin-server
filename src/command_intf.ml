type command =
  | Ping
  | SetBranch
  (* Store *)
  | Set
  | Find
  | Remove
  | FindTree
  | SetTree
  (* Tree *)
  | EmptyTree
  | TreeAdd
  | TreeRemove
[@@deriving irmin]

type t = command

module type S = sig
  module Store : Irmin_pack_layered.S

  module Tree : Tree.S with module Private.Store = Store

  type t = command

  type context = {
    conn : Conn.t;
    repo : Store.Repo.t;
    mutable store : Store.t;
    trees : (int, Store.tree) Hashtbl.t;
  }

  type f = Conn.t -> context -> Args.t -> Return.t Lwt.t

  val n_args : command -> int

  val n_results : command -> int

  val commands : (command * (int * int * f)) list
end

module type Command = sig
  type t = command

  val name : t -> string

  val of_name : string -> t

  module type S = S

  module Make (Store : Irmin_pack_layered.S) : S with module Store = Store
end
