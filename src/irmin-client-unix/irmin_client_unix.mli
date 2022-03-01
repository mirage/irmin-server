module Error = Irmin_client.Error
module IO : Irmin_client.Client.IO

module Make_ext
    (Codec : Irmin_server_internal.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) :
  Irmin_client.S
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
  Irmin_client.S
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
  Irmin_client.S
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
  module Make (Store : Irmin.Generic_key.S) :
    Irmin.Generic_key.S
      with module Schema = Store.Schema
       and type commit_key = Store.commit_key

  module Make_json (Store : Irmin.Generic_key.S) :
    Irmin.Generic_key.S
      with module Schema = Store.Schema
       and type commit_key = Store.commit_key
end
