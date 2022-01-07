include Irmin_server_intf
open Irmin_server_types
module Error = Error

module type S = Server.S

module Make (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) = struct
  module X = struct
    include Command
    include Command.Make (Codec) (Store)
  end

  include Server.Make (X)
end

module Make_bin (Store : Irmin.Generic_key.S) = Make (Conn.Codec.Bin) (Store)
module Make_json (Store : Irmin.Generic_key.S) = Make (Conn.Codec.Json) (Store)
