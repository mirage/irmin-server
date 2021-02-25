type t = Unknown of string | Ping | SetBranch | Get [@@deriving irmin]

type command = t

module type Command = sig
  type t = command

  val t : t Irmin.Type.t

  val name : t -> string

  val of_name : string -> t
end
