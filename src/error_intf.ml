type error = [ `Msg of string ]

type t = error

type 'a result = ('a, error) Result.t

exception Error of int * string

exception Unwrap of string

module type Error = sig
  type t = error

  type 'a result = ('a, error) Result.t

  exception Error of int * string

  exception Unwrap of string

  val raise_error : int -> string -> 'a

  val unwrap : string -> 'a result -> 'a

  val of_string : string -> t

  val to_string : t -> string
end
