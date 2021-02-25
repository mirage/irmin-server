type error = [ `Msg of string ]

type t = error

exception Error of int * string

module type Error = sig
  type t = error

  exception Error of int * string

  val raise_error : int -> string -> 'a
end
