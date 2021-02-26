type command = Ping | SetBranch | Find | Set | Remove [@@deriving irmin]

type t = command

module type S = sig
  module Store : Irmin.S

  type t = command

  type context = { conn : Conn.t; repo : Store.Repo.t; mutable store : Store.t }

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

  module Make (Store : Irmin.S) : S with module Store = Store
end
