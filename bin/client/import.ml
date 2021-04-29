type client =
  | S : ((module Irmin_server.Client.S with type t = 'a) * 'a Lwt.t) -> client
