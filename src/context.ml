open Lwt.Syntax
open Lwt.Infix

module Make (St : Command_intf.STORE with type key = string list) = struct
  type context = {
    conn : Conn.t;
    repo : St.Repo.t;
    mutable branch : St.branch;
    mutable store : St.t;
    trees : (int, St.tree) Hashtbl.t;
  }

  module Commit = Irmin.Private.Commit.Make (St.Hash)

  module type CMD = sig
    module Req : sig
      type t

      val t : t Irmin.Type.t
    end

    module Res : sig
      type t

      val t : t Irmin.Type.t
    end

    val name : string

    val run : Conn.t -> context -> Req.t -> Res.t Return.t Lwt.t
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
    | None -> Error.raise_error "unknown tree"
end
