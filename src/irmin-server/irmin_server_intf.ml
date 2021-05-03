open Irmin_server_types
module Error = Error

module type Irmin_server = sig
  module type S = Server.S

  module Error = Error

  module Make (Store : Irmin_server_types.Command.STORE) :
    S with module Store = Store
end
