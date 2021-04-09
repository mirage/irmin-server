module Error = Error
module Client = Client
module Server = Server
module Command = Command
module Cli = Cli
module Return = Return
include Irmin_server_intf

module Make (Conf : sig
  val entries : int

  val stable_hash : int
end)
(M : Irmin.Metadata.S)
(C : Irmin.Contents.S)
(B : Irmin.Branch.S)
(H : Irmin.Hash.S) =
struct
  module Store =
    Irmin_pack_layered.Make (Conf) (M) (C) (Irmin.Path.String_list) (B) (H)

  module Command = struct
    include Command
    include Command.Make (Store)
  end

  module Client = Client.Make (Command)
  module Server = Server.Make (Command)
end

module Make_ext (Conf : sig
  val entries : int

  val stable_hash : int
end)
(M : Irmin.Metadata.S)
(C : Irmin.Contents.S)
(B : Irmin.Branch.S)
(H : Irmin.Hash.S)
(N : Irmin.Private.Node.S
       with type metadata = unit
        and type hash = H.t
        and type step = string)
(Cm : Irmin.Private.Commit.S with type hash = H.t) =
struct
  module Store =
    Irmin_pack_layered.Make_ext (Conf) (Irmin.Metadata.None) (C)
      (Irmin.Path.String_list)
      (B)
      (H)
      (N)
      (Cm)

  module Command = struct
    include Command
    include Command.Make (Store)
  end

  module Client = Client.Make (Command)
  module Server = Server.Make (Command)
end

module KV (C : Irmin.Contents.S) =
  Make (Conf.Default) (Irmin.Metadata.None) (C) (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)
