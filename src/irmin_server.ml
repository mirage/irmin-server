module Error = Error
module Client = Client
module Server = Server
module Command = Command
module Cli = Cli
module Return = Return
include Irmin_server_intf

module Make
    (Version : Irmin_pack.Version.S) (Conf : sig
      val entries : int

      val stable_hash : int
    end)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module Maker = Irmin_pack.Maker (Version) (Conf)
  module Store = Maker.Make (M) (C) (Irmin.Path.String_list) (B) (H)

  module Command = struct
    include Command
    include Command.Make (Store)
  end

  module Client = Client.Make (Command)
  module Server = Server.Make (Command)
end

module Make_ext
    (Version : Irmin_pack.Version.S) (Conf : sig
      val entries : int

      val stable_hash : int
    end)
    (N : Irmin.Private.Node.Maker)
    (Cm : Irmin.Private.Commit.Maker)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module Maker = Irmin_pack.Maker_ext (Version) (Conf) (N) (Cm)
  module Store = Maker.Make (M) (C) (Irmin.Path.String_list) (B) (H)

  module Command = struct
    include Command
    include Command.Make (Store)
  end

  module Client = Client.Make (Command)
  module Server = Server.Make (Command)
end

module Make_layered (Conf : sig
  val entries : int

  val stable_hash : int
end)
(N : Irmin.Private.Node.Maker)
(Cm : Irmin.Private.Commit.Maker)
(M : Irmin.Metadata.S)
(C : Irmin.Contents.S)
(B : Irmin.Branch.S)
(H : Irmin.Hash.S) =
struct
  module Maker = Irmin_pack_layered.Maker_ext (Conf) (N) (Cm)
  module Store = Maker.Make (M) (C) (Irmin.Path.String_list) (B) (H)

  module Command = struct
    include Command
    include Command.Make (Store)
  end

  module Client = Client.Make (Command)
  module Server = Server.Make (Command)
end

module KV (V : Irmin_pack.Version.S) (C : Irmin.Contents.S) =
  Make (V) (Conf.Default) (Irmin.Metadata.None) (C) (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)
