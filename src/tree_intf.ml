module type LOCAL = sig
  type t

  type contents

  type key

  type hash

  type step

  val t : t Irmin.Type.t

  val empty : t

  val of_contents : contents -> t

  val add : t -> key -> contents -> t Lwt.t

  val add_tree : t -> key -> t -> t Lwt.t

  val find : t -> key -> contents option Lwt.t

  val find_tree : t -> key -> t option Lwt.t

  val remove : t -> key -> t Lwt.t

  val mem : t -> key -> bool Lwt.t

  val mem_tree : t -> key -> bool Lwt.t

  val update : t -> key -> (contents option -> contents option) -> t Lwt.t

  val update_tree : t -> key -> (t option -> t option) -> t Lwt.t

  val kind : t -> key -> [ `Contents | `Node ] option Lwt.t

  val destruct : t -> [ `Contents of hash | `Node of (step * t) list ] Lwt.t
end

module type S = sig
  module Private : sig
    module Store : Irmin_pack_layered.S with type key = string list
  end

  open Private

  module Local :
    LOCAL
      with type contents = Store.contents
       and type key = Store.key
       and type hash = Store.hash
       and type step = Store.Key.step

  type t = Hash of Store.Hash.t | ID of int | Local of Local.t
  [@@deriving irmin]
end

module type Tree = sig
  module type S = S

  module Make (S : Irmin_pack_layered.S with type key = string list) :
    S with module Private.Store = S and type Local.t = S.tree
end
