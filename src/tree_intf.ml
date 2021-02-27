module type S = sig
  module Private : sig
    module Store : Irmin.S
  end

  open Private

  module Local : sig
    type t

    val t : t Irmin.Type.t

    val empty : t

    val of_contents : Store.contents -> t

    val add : t -> Store.key -> Store.contents -> t Lwt.t

    val add_tree : t -> Store.key -> t -> t Lwt.t

    val find : t -> Store.key -> Store.contents option Lwt.t

    val find_tree : t -> Store.key -> t option Lwt.t

    val remove : t -> Store.key -> t Lwt.t

    val mem : t -> Store.key -> bool Lwt.t

    val mem_tree : t -> Store.key -> bool Lwt.t

    val update :
      t ->
      Store.key ->
      (Store.contents option -> Store.contents option) ->
      t Lwt.t

    val update_tree : t -> Store.key -> (t option -> t option) -> t Lwt.t

    val kind : t -> Store.key -> [ `Contents | `Node ] option Lwt.t

    val destruct :
      t ->
      [ `Contents of Store.hash | `Node of (Store.Key.step * t) list ] Lwt.t
  end

  type t = Hash of Store.Hash.t | ID of int | Local of Local.t
  [@@deriving irmin]
end

module type Tree = sig
  module type S = S

  module Make (S : Irmin.S) :
    S with module Private.Store = S and type Local.t = S.tree
end
