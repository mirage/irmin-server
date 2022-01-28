open Irmin_server_internal
module Error = Error

module type S = Client.S

module Client = Client

module Make_ext
    (Codec : Irmin_server_internal.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) =
struct
  module Command = struct
    include Command
    include Command.Make (Codec) (Store)
  end

  include Client.Make (Command)
end

module Make (Store : Irmin.Generic_key.S) = Make_ext (Conn.Codec.Bin) (Store)

module Make_json (Store : Irmin.Generic_key.S) =
  Make_ext (Conn.Codec.Json) (Store)
