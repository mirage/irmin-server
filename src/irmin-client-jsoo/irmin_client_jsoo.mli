module Error = Irmin_client.Error
module IO : Irmin_client.Client.IO

module Info (I : Irmin.Info.S) : sig
  include Irmin.Info.S with type t = I.t

  val init : ?author:string -> ?message:string -> int64 -> t
  val v : ?author:string -> ('b, Format.formatter, unit, f) format4 -> 'b
end

module type S = sig
  include Irmin_client.S

  val connect :
    ?ctx:IO.ctx -> ?batch_size:int -> ?tls:bool -> uri:Uri.t -> unit -> t Lwt.t
end

module Make_ext
    (Codec : Irmin_server_internal.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) :
  S
    with type hash = Store.hash
     and type contents = Store.contents
     and type branch = Store.branch
     and type path = Store.path
     and type step = Store.step
     and type metadata = Store.metadata
     and type slice = Store.slice
     and module Schema = Store.Schema
     and type Private.Store.tree = Store.tree
     and module IO = IO

module Make (Store : Irmin.Generic_key.S) :
  S
    with type hash = Store.hash
     and type contents = Store.contents
     and type branch = Store.branch
     and type path = Store.path
     and type step = Store.step
     and type metadata = Store.metadata
     and type slice = Store.slice
     and module Schema = Store.Schema
     and type Private.Store.tree = Store.tree
     and module IO = IO

module Make_json (Store : Irmin.Generic_key.S) :
  S
    with type hash = Store.hash
     and type contents = Store.contents
     and type branch = Store.branch
     and type path = Store.path
     and type step = Store.step
     and type metadata = Store.metadata
     and type slice = Store.slice
     and module Schema = Store.Schema
     and type Private.Store.tree = Store.tree
     and module IO = IO

module Store : sig
  val config :
    ?batch_size:int -> ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config

  module Make (Store : Irmin.Generic_key.S) :
    Irmin.Generic_key.S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type contents_key = Store.contents_key
       and type commit_key = Store.commit_key
       and type node_key = Store.node_key

  module Make_json (Store : Irmin.Generic_key.S) :
    Irmin.Generic_key.S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type contents_key = Store.contents_key
       and type commit_key = Store.commit_key
       and type node_key = Store.node_key
end
