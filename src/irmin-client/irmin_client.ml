module type S = Client.S

module Error = Irmin_server_types.Error
module Client = Client

module Make
    (Codec : Irmin_server_types.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) =
struct
  module Command = struct
    include Irmin_server_types.Command
    include Irmin_server_types.Command.Make (Codec) (Store)
  end

  include Client.Make (Command)
end

module Make_bin (Store : Irmin.Generic_key.S) =
  Make (Irmin_server_types.Conn.Codec.Bin) (Store)

module Make_json (Store : Irmin.Generic_key.S) =
  Make (Irmin_server_types.Conn.Codec.Json) (Store)
