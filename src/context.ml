open Lwt.Syntax
open Lwt.Infix

module Make (St : Irmin_pack_layered.S) = struct
  type context = {
    conn : Conn.t;
    repo : St.Repo.t;
    mutable store : St.t;
    trees : (int, St.tree) Hashtbl.t;
  }

  type f = Conn.t -> context -> [ `Read ] Args.t -> unit Lwt.t

  module type CMD = sig
    type req

    type res

    val args : int * int

    val name : string

    module Server : sig
      val recv : context -> [ `Read ] Args.t -> req Error.result Lwt.t

      val handle : Conn.t -> context -> req -> res Return.t Lwt.t
    end

    module Client : sig
      val send : [ `Write ] Args.t -> req -> unit Lwt.t

      val recv : [ `Read ] Args.t -> res Error.result Lwt.t
    end
  end

  let cmd (module C : CMD) = (C.name, (module C : CMD))

  module Tree = Tree.Make (St)

  let resolve_tree ctx tree =
    let* id, tree =
      match tree with
      | Tree.ID x -> Lwt.return @@ (x, Hashtbl.find_opt ctx.trees x)
      | Hash x ->
          St.Tree.of_hash ctx.repo (`Node x) >|= fun x -> (Random.bits (), x)
      | Local x -> Lwt.return (Random.bits (), Some x)
    in
    match tree with
    | Some t -> Lwt.return (id, t)
    | None -> Error.raise_error 0 "unknown tree"
end
