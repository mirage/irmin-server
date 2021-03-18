module type S = sig
  module Store : Irmin_pack_layered.S with type key = string list

  module Command : Command.S with module Store = Store

  module Server : Server.S with module Store = Store

  module Client :
    Client.S
      with type hash = Store.hash
       and type contents = Store.contents
       and type branch = Store.branch
       and type key = Store.key
end

module Conf = struct
  let entries = 32

  let stable_hash = 256
end

module type Irmin_server = sig
  module Command = Command
  module Error = Error
  module Server = Server
  module Client = Client
  module Cli = Cli
  module Return = Return

  module type S = S

  module Make (H : Irmin.Hash.S) (C : Irmin.Contents.S) (B : Irmin.Branch.S) :
    S
      with type Store.hash = H.t
       and type Store.contents = C.t
       and type Store.branch = B.t
       and type Store.key = string list

  module KV (C : Irmin.Contents.S) :
    S
      with type Store.hash = Irmin.Hash.BLAKE2B.t
       and type Store.contents = C.t
       and type Store.branch = string
       and type Store.key = string list
end
