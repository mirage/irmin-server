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
(H : Irmin.Hash.S)
(C : Irmin.Contents.S)
(B : Irmin.Branch.S) =
struct
  module Store =
    Irmin_pack_layered.Make (Conf) (Irmin.Metadata.None) (C)
      (Irmin.Path.String_list)
      (B)
      (H)

  module Command = struct
    include Command
    include Command.Make (Store)
  end

  module Client = Client.Make (Command)
  module Server = Server.Make (Command)
end

module KV (C : Irmin.Contents.S) =
  Make (Conf.Default) (Irmin.Hash.BLAKE2B) (C) (Irmin.Branch.String)
