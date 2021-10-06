module type LOCAL = sig
  type t

  type contents

  type key

  type hash

  type node

  type step

  type metadata

  val t : t Irmin.Type.t

  type concrete =
    [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

  val concrete_t : concrete Irmin.Type.t

  val of_concrete : concrete -> t

  val to_concrete : t -> concrete Lwt.t

  val empty : t

  val of_contents : ?metadata:metadata -> contents -> t

  val of_node : node -> t

  val add : t -> key -> ?metadata:metadata -> contents -> t Lwt.t

  val add_tree : t -> key -> t -> t Lwt.t

  val find : t -> key -> contents option Lwt.t

  val find_tree : t -> key -> t option Lwt.t

  val remove : t -> key -> t Lwt.t

  val mem : t -> key -> bool Lwt.t

  val mem_tree : t -> key -> bool Lwt.t

  val update :
    t ->
    key ->
    ?metadata:metadata ->
    (contents option -> contents option) ->
    t Lwt.t

  val update_tree : t -> key -> (t option -> t option) -> t Lwt.t

  val kind : t -> key -> [ `Contents | `Node ] option Lwt.t

  val destruct : t -> [ `Contents of hash | `Node of (step * t) list ] Lwt.t

  val list :
    t ->
    ?offset:int ->
    ?length:int ->
    ?cache:bool ->
    key ->
    (step * t) list Lwt.t

  val diff : t -> t -> (key * (contents * metadata) Irmin.Diff.t) list Lwt.t

  val merge : t Irmin.Merge.t

  type elt = [ `Node of node | `Contents of contents * metadata ]
  (** The type for tree elements. *)

  val v : elt -> t
  (** General-purpose constructor for trees. *)
end

module type S = sig
  module Private : sig
    module Store : Irmin.S
  end

  open Private

  module Local :
    LOCAL
      with type contents = Store.contents
       and type key = Store.key
       and type hash = Store.hash
       and type step = Store.Key.step
       and type t = Store.tree

  type t = Hash of Store.Hash.t | ID of int | Local of Local.concrete
  [@@deriving irmin]
end

module type Tree = sig
  module type S = S

  module type LOCAL = LOCAL

  module Make (S : Irmin.S) : S with module Private.Store = S
end
