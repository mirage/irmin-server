module Make (St : Irmin_pack_layered.S) = struct
  type context = {
    conn : Conn.t;
    repo : St.Repo.t;
    mutable store : St.t;
    trees : (int, St.tree) Hashtbl.t;
  }

  type f = Conn.t -> context -> Args.t -> unit Lwt.t

  module type CMD = sig
    type req

    type res

    val args : int * int

    val name : string

    module Server : sig
      val handle : Conn.t -> context -> Args.t -> res Return.t Lwt.t
    end

    module Client : sig
      val send : Args.t -> req -> unit Lwt.t

      val recv : Args.t -> res Error.result Lwt.t
    end
  end
end
