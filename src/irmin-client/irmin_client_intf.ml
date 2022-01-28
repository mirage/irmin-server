open Irmin_server_internal
module Error = Error

module type Irmin_client = sig
  module type S = Client.S

  module Error = Error
  module Client = Client

  module Make_ext (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) :
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
end
