open Irmin_server_internal
module Error = Error

module type S = Client.S

module Client = Client

type addr = Client_intf.addr

module Make_ext
    (IO : Client.IO)
    (Codec : Irmin_server_internal.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) =
struct
  include Client.Make (IO) (Codec) (Store)
end

module Make (IO : Client.IO) (Store : Irmin.Generic_key.S) =
  Make_ext (IO) (Conn.Codec.Bin) (Store)

module Make_json (IO : Client.IO) (Store : Irmin.Generic_key.S) =
  Make_ext (IO) (Conn.Codec.Json) (Store)
