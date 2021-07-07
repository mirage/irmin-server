open Irmin_server_types
module Error = Error

module type Irmin_client = sig
  module type S = Client.S

  module Client = Client

  module Make (Store : Irmin.S) :
    S
      with type hash = Store.hash
       and type contents = Store.contents
       and type branch = Store.branch
       and type key = Store.key
       and type step = Store.step
       and type metadata = Store.metadata
       and type slice = Store.slice
       and module Schema = Store.Schema
       and type Private.Store.tree = Store.tree
end
