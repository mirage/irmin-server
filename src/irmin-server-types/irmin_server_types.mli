module Command = Command
module Error = Error
module Cli = Cli
module Conn = Conn
module Return = Return
module Stats = Stats
module Tree = Tree
module Handshake = Handshake
module Request = Request
module Response = Response

module Conf : sig
  module type S = sig
    val entries : int

    val stable_hash : int
  end

  module Default : S
end
