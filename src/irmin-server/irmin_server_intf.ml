open Irmin_server_types
module Error = Error

module type Irmin_server = sig
  module type S = Server.S

  module Error = Error

  module Make (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) :
    S with module Store = Store

  module Make_bin (Store : Irmin.Generic_key.S) : S with module Store = Store
  module Make_json (Store : Irmin.Generic_key.S) : S with module Store = Store
end
