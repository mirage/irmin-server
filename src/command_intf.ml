type command = Ping | SetBranch | Find | Set | Remove [@@deriving irmin]

type t = command

module type S = sig
  module Store : Irmin.S

  type t = command

  type client = { conn : Conn.t; repo : Store.Repo.t; mutable store : Store.t }

  module Return : sig
    type t

    val make : int -> client -> t Lwt.t

    val ok : client -> t Lwt.t

    val err : client -> string -> t Lwt.t

    val v : client -> 'a Irmin.Type.t -> 'a -> t Lwt.t

    val write : 'a Irmin.Type.t -> 'a -> t -> t Lwt.t

    val check : t -> unit
  end

  type f = client -> Args.t -> Return.t Lwt.t

  val n_args : command -> int

  val commands : (command * (int * f)) list
end

module type Command = sig
  type t = command

  val name : t -> string

  val of_name : string -> t

  module type S = S

  module Make (Store : Irmin.S) : S with module Store = Store
end
