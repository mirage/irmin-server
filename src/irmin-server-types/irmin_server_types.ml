module Error = Error
module Command = Command
module Cli = Cli
module Conn = Conn
module Return = Return
module Stats = Stats
module Tree = Tree
module Handshake = Handshake
module Request = Request
module Response = Response

module Conf = struct
  module type S = sig
    val entries : int

    val stable_hash : int
  end

  module Default : S = struct
    let entries = 32

    let stable_hash = 32
  end
end
