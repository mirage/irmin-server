open Irmin_server_internal
module Error = Error

module type Irmin_client = sig
  module type S = Client.S

  module Error = Error
  module Client = Client

  type addr = Client.addr

  module Make_ext
      (IO : Client.IO)
      (Codec : Conn.Codec.S)
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

  module Make (IO : Client.IO) (Store : Irmin.Generic_key.S) :
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

  module Make_json (IO : Client.IO) (Store : Irmin.Generic_key.S) :
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

    module Make
        (IO : Client.IO)
        (Codec : Conn.Codec.S)
        (Store : Irmin.Generic_key.S) :
      Irmin.Generic_key.S
        with module Schema = Store.Schema
         and type commit_key = Store.commit_key
  end
end
