module type S = Client.S

module Error = Irmin_server_types.Error
module Client = Client

module Make (Store : Irmin_server_types.Command.STORE) = struct
  module Command = struct
    include Irmin_server_types.Command
    include Irmin_server_types.Command.Make (Store)
  end

  include Client.Make (Command)
end
