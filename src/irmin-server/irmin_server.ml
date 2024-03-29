include Irmin_server_intf
open Irmin_server_internal
module Error = Error

module type S = Server.S

module Make_ext (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) = struct
  include Server.Make (Codec) (Store)
end

module Make (Store : Irmin.Generic_key.S) = Make_ext (Conn.Codec.Bin) (Store)

module Make_json (Store : Irmin.Generic_key.S) =
  Make_ext (Conn.Codec.Json) (Store)
