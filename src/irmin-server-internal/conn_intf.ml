module Codec = struct
  module type S = sig
    val encode : 'a Irmin.Type.t -> 'a -> string
    val decode : 'a Irmin.Type.t -> string -> ('a, [ `Msg of string ]) result
  end
end

type t = {
  flow : Conduit_lwt_unix.flow;
  ic : Conduit_lwt_unix.ic;
  oc : Conduit_lwt_unix.oc;
  buffer : bytes;
}

module type S = sig
  type nonrec t = t

  val v :
    ?buffer_size:int ->
    Conduit_lwt_unix.flow ->
    Conduit_lwt_unix.ic ->
    Conduit_lwt_unix.oc ->
    t
  (** Create a new connection using [flow], [ic] and [oc] from conduit *)

  val is_closed : t -> bool
  (** Check if the underlying channel is closed *)

  val read : buffer:bytes -> t -> 'a Irmin.Type.t -> 'a Error.result Lwt.t
  (** Read the next message *)

  val write : t -> 'a Irmin.Type.t -> 'a -> unit Lwt.t
  (** Write a message *)

  val ok : t -> unit Lwt.t
  (** Send "OK" message with [unit] response *)

  val err : t -> string -> unit Lwt.t
  (** Send error message *)

  module Handshake : sig
    module V1 : sig
      val version : string
      val send : (module Irmin.Generic_key.S) -> t -> unit Lwt.t
      val check : (module Irmin.Generic_key.S) -> t -> bool Lwt.t
    end
  end

  module Request : sig
    type header = { command : string }

    val v_header : command:string -> header
    val write_header : t -> header -> unit Lwt.t
    val read_header : t -> header Lwt.t
  end

  module Response : sig
    type header = { status : int }

    val v_header : status:int -> header
    val write_header : t -> header -> unit Lwt.t
    val read_header : t -> header Lwt.t
    val is_error : header -> bool
    val get_error : bytes -> t -> header -> string option Lwt.t
  end

  module Return : sig
    type conn = t
    type 'a t = { status : int; conn : conn }

    val make : int -> conn -> 'a t Lwt.t
    val err : conn -> string -> 'a t Lwt.t
    val write : 'a Irmin.Type.t -> 'a -> 'a t -> 'a t Lwt.t
    val v : conn -> 'a Irmin.Type.t -> 'a -> 'a t Lwt.t
    val ok : conn -> unit t Lwt.t

    val result :
      conn -> 'a Irmin.Type.t -> ('a, [ `Msg of string ]) Result.t -> 'a t Lwt.t

    val finish : 'a t -> unit Lwt.t
  end
end

module type Sigs = sig
  type nonrec t = t

  module Codec : sig
    module type S = Codec.S

    module Bin : S
    module Json : S
  end

  module type S = S

  module Make (C : Codec.S) : S
end
