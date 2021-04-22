module type S = sig
  module Store : Command.STORE

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
  module Default = struct
    let entries = 32

    let stable_hash = 32
  end
end

module type Irmin_server = sig
  module Command = Command
  module Error = Error
  module Server = Server
  module Client = Client
  module Cli = Cli
  module Return = Return

  module type S = S

  module Conf = Conf

  module Make
      (V : Irmin_pack.Version.S) (Conf : sig
        val entries : int

        val stable_hash : int
      end)
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) :
    S
      with type Store.hash = H.t
       and type Store.contents = C.t
       and type Store.branch = B.t
       and type Store.key = string list
       and type Store.step = string
       and type Store.metadata = M.t

  module Make_ext
      (V : Irmin_pack.Version.S) (Conf : sig
        val entries : int

        val stable_hash : int
      end)
      (N : Irmin.Private.Node.Maker)
      (Cm : Irmin.Private.Commit.Maker)
      (M : Irmin.Metadata.S with type t = unit)
      (C : Irmin.Contents.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) :
    S
      with type Store.hash = H.t
       and type Store.contents = C.t
       and type Store.branch = B.t
       and type Store.key = string list
       and type Store.step = string
       and type Store.metadata = M.t

  module Make_layered (Conf : sig
    val entries : int

    val stable_hash : int
  end)
  (N : Irmin.Private.Node.Maker)
  (Cm : Irmin.Private.Commit.Maker)
  (M : Irmin.Metadata.S with type t = unit)
  (C : Irmin.Contents.S)
  (B : Irmin.Branch.S)
  (H : Irmin.Hash.S) :
    S
      with type Store.hash = H.t
       and type Store.contents = C.t
       and type Store.branch = B.t
       and type Store.key = string list
       and type Store.step = string
       and type Store.metadata = M.t

  module KV (V : Irmin_pack.Version.S) (C : Irmin.Contents.S) :
    S
      with type Store.hash = Irmin.Hash.BLAKE2B.t
       and type Store.contents = C.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string
end
