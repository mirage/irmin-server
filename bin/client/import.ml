type client =
  | S : ((module Irmin_client_unix.S with type t = 'a) * 'a) -> client
