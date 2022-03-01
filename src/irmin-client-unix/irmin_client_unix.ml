open Irmin_server_internal
module Error = Error
module IO = IO

module Make_ext
    (Codec : Irmin_server_internal.Conn.Codec.S)
    (Store : Irmin.Generic_key.S) =
struct
  include Irmin_client.Make_ext (IO) (Codec) (Store)
end

module Make (Store : Irmin.Generic_key.S) = Make_ext (Conn.Codec.Bin) (Store)

module Make_json (Store : Irmin.Generic_key.S) =
  Make_ext (Conn.Codec.Json) (Store)

module Store = struct
  module Make (Store : Irmin.Generic_key.S) =
    Irmin_client.Store.Make (IO) (Conn.Codec.Bin) (Store)

  module Make_json (Store : Irmin.Generic_key.S) =
    Irmin_client.Store.Make (IO) (Conn.Codec.Json) (Store)
end
