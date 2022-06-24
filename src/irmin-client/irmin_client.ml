open Irmin_server_internal
include Client
module Error = Error

module Make_codec
    (IO : Client.IO)
    (Codec : Irmin_server_internal.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) =
struct
  include Client.Make (IO) (Codec) (Store)
end

module Make (IO : Client.IO) (Store : Irmin.Generic_key.S) =
  Make_codec (IO) (Conn.Codec.Bin) (Store)

module Make_json (IO : Client.IO) (Store : Irmin.Generic_key.S) =
  Make_codec (IO) (Conn.Codec.Json) (Store)
