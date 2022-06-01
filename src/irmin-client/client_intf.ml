open Irmin_server_internal

type addr =
  [ `TLS of [ `Hostname of string ] * [ `IP of Ipaddr.t ] * [ `Port of int ]
  | `TCP of [ `IP of Ipaddr.t ] * [ `Port of int ]
  | `Ws of ([ `IP of Ipaddr.t ] * [ `Port of int ]) option * string
  | `Unix_domain_socket of [ `File of string ] ]

module type IO = sig
  include Conn.IO

  type ctx

  val default_ctx : ctx lazy_t
  val connect : ctx:ctx -> addr -> (ic * oc) Lwt.t
  val close : ic * oc -> unit Lwt.t
end

module type S = sig
  include Irmin.Generic_key.S

  val connect :
    ?batch_size:int -> ?tls:bool -> ?hostname:string -> Uri.t -> repo Lwt.t

  val reconnect : repo -> unit Lwt.t

  val uri : repo -> Uri.t
  (** Get the URI the client is connected to *)

  val close : repo -> unit Lwt.t
  (** Close connection to the server *)

  val dup : repo -> repo Lwt.t
  (** Duplicate a client. This will create a new connection with the same configuration *)

  val ping : repo -> unit Error.result Lwt.t
  (** Ping the server *)

  val export : ?depth:int -> repo -> slice Error.result Lwt.t
  val import : repo -> slice -> unit Error.result Lwt.t
  val current_branch : t -> branch Error.result Lwt.t

  module Batch : sig
    module Tree : sig
      include
        Irmin_server_internal.Tree.S
          with type concrete = Tree.concrete
           and type kinded_key = Tree.kinded_key

      val empty : repo -> t Error.result Lwt.t

      val save :
        repo ->
        t ->
        [ `Contents of contents_key | `Node of node_key ] Error.result Lwt.t

      val to_local : repo -> t -> tree Error.result Lwt.t
      val of_local : tree -> t Lwt.t
    end

    type batch_contents =
      [ `Hash of hash | `Value of contents ] * metadata option

    type t =
      (path * [ `Contents of batch_contents | `Tree of Tree.t ] option) list

    val apply : repo -> tree -> t -> Tree.t Error.result Lwt.t
    val find : t -> path -> batch_contents option
    val find_tree : t -> path -> Tree.t option
    val mem : t -> path -> bool
    val mem_tree : t -> path -> bool
    val remove : t -> path -> t
    val add : t -> path -> ?metadata:metadata -> contents -> t
    val add_hash : t -> path -> ?metadata:metadata -> hash -> t
    val add_tree : t -> path -> Tree.t -> t
  end

  (*module Commit : sig
      type key

      val key_t : key Irmin.Type.t

      val v :
        t -> info:Info.f -> parents:key list -> tree -> commit Error.result Lwt.t
      (** Create a new commit
          NOTE: this will invalidate all intermediate trees *)

      val of_key : t -> key -> commit option Error.result Lwt.t
      val of_hash : t -> hash -> commit option Error.result Lwt.t

      val key : commit -> key
      (** Get commit key *)

      val hash : t -> commit -> hash option Error.result Lwt.t
      (** Get commit hash *)

      val parents : commit -> key list
      (** The commit parents. *)

      val info : commit -> Info.t
      (** The commit info. *)

      val t : commit Irmin.Type.t
      (** [t] is the value type for {!t}. *)

      val hash_t : hash Irmin.Type.t
      (** [hash_t] is the value type for {!hash}. *)

      val tree : t -> commit -> tree
      (** Commit tree *)

      type t = commit
    end

    module Contents : sig
      type key = contents_key

      val of_hash : t -> hash -> contents option Error.result Lwt.t
      (** Find the contents associated with a hash *)

      val exists : t -> contents -> bool Error.result Lwt.t
      (** Check if [contents] exists in the store already *)

      val save : t -> contents -> contents_key Error.result Lwt.t
      (** Save value to store without associating it with a path *)

      include Irmin.Contents.S with type t = contents
    end

    module Branch : sig
      val set_current : t -> branch -> unit Error.result Lwt.t
      (** Set the current branch for a single connection *)

      val get_current : t -> branch Error.result Lwt.t
      (** Get the branch for a connection *)

      val get : ?branch:branch -> t -> commit option Error.result Lwt.t
      (** Get the head commit for the given branch, or the current branch if none is specified *)

      val set : ?branch:branch -> t -> commit -> unit Error.result Lwt.t
      (** Set the head commit for the given branch, or the current branch if none is specified *)

      val remove : t -> branch -> unit Error.result Lwt.t
      (** Delete a branch *)

      include Irmin.Branch.S with type t = branch
    end

    module Tree : sig
      type key

      val key_t : key Irmin.Type.t

      val split : tree -> t * tree * Batch.t
      (** Get private fields from [Tree.t] *)

      val v : t -> ?batch:Batch.t -> tree -> Batch.Tree.t
      (** Create a new tree *)

      val of_key : t -> key -> tree
      (** Create a tree from a key that specifies a tree that already exists in the store *)

      val empty : t -> tree
      (** Create a new, empty tree *)

      val clear : tree -> unit Error.result Lwt.t
      (** Clear caches on the server for a given tree *)

      val flush : tree -> tree Error.result Lwt.t
      (** Apply batch updates, ignoring the configured `batch_size` *)

      val key : tree -> key Error.result Lwt.t
      (** Get key of tree *)

      val build : t -> ?tree:tree -> Batch.t -> Batch.Tree.t Error.result Lwt.t
      (** [build store ~tree batch] performs a batch update of [tree], or
          an empty tree if not specified *)

      val add :
        tree -> path -> ?metadata:metadata -> contents -> tree Error.result Lwt.t
      (** Add contents to a tree, this may be batched so the update on the server
          could be delayed *)

      val add' : tree -> path -> contents -> tree Error.result Lwt.t
      (** Non-batch version of [add] *)

      val add_tree : tree -> path -> tree -> tree Error.result Lwt.t

      val add_tree' : tree -> path -> tree -> tree Error.result Lwt.t
      (** Non-batch version of [add_tree] *)

      val batch_update : tree -> Batch.t -> tree Error.result Lwt.t
      (** Batch update tree *)

      val find : tree -> path -> contents option Error.result Lwt.t
      (** Find the value associated with the given path *)

      val find_tree : tree -> path -> tree option Error.result Lwt.t
      (** Find the tree associated with the given path *)

      val remove : tree -> path -> tree Error.result Lwt.t
      (** Remove value from a tree, returning a new tree *)

      val cleanup : tree -> unit Error.result Lwt.t
      (** Invalidate a tree, this frees the tree on the server side *)

      val cleanup_all : t -> unit Error.result Lwt.t
      (** Cleanup all trees *)

      val mem : tree -> path -> bool Error.result Lwt.t
      (** Check if a path is associated with a value *)

      val mem_tree : tree -> path -> bool Error.result Lwt.t
      (** Check if a path is associated with a tree *)

      val list :
        tree ->
        path ->
        (Path.step * [ `Contents | `Tree ]) list Error.result Lwt.t
      (** List entries at the specified root *)

      val merge : old:tree -> tree -> tree -> tree Error.result Lwt.t
      (** Three way merge *)

      val hash : tree -> hash Error.result Lwt.t

      val save :
        tree ->
        [ `Contents of contents_key | `Node of node_key ] Error.result Lwt.t

      module Local : sig
        type t = tree

        include module type of Tree
      end

      val to_local : tree -> Local.t Error.result Lwt.t
      (** Exchange [tree], which may be a hash or ID, for a tree
          NOTE: this will encode the full tree and should be avoided if possible  *)

      val of_local : t -> Local.t -> tree Lwt.t
      (** Convert a local tree into a remote tree *)

      type t = tree
    end

    val find : t -> path -> contents option Error.result Lwt.t
    (** Find the value associated with a path, if it exists *)

    val find_tree : t -> path -> Tree.t option Error.result Lwt.t
    (** Find the tree associated with a path, if it exists *)

    val set : t -> info:Info.f -> path -> contents -> unit Error.result Lwt.t
    (** Associate a new value with the given path *)

    val test_and_set :
      t ->
      info:Info.f ->
      path ->
      test:contents option ->
      set:contents option ->
      unit Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value *)

    val remove : t -> info:Info.f -> path -> unit Error.result Lwt.t
    (** Remove a value from the store *)

    val set_tree : t -> info:Info.f -> path -> Tree.t -> Tree.t Error.result Lwt.t
    (** Set a tree at the given path *)

    val test_and_set_tree :
      t ->
      info:Info.f ->
      path ->
      test:Tree.t option ->
      set:Tree.t option ->
      Tree.t option Error.result Lwt.t
    (** Set a value only if the [test] parameter matches the existing value *)

    val mem : t -> path -> bool Error.result Lwt.t
    (** Check if the given path has an associated value *)

    val mem_tree : t -> path -> bool Error.result Lwt.t
    (** Check if the given path has an associated tree *)

    val merge : t -> info:Info.f -> branch -> unit Error.result Lwt.t
    (** Merge the current branch with the provided branch *)

    val merge_commit : t -> info:Info.f -> Commit.t -> unit Error.result Lwt.t
    (** Merge the current branch with the provided commit *)

    val last_modified : t -> path -> Commit.t list Error.result Lwt.t
    (** Get a list of commits that modified the specified path *)*)
end

module type Client = sig
  module type S = S

  type nonrec addr = addr

  module type IO = IO

  val config :
    ?batch_size:int -> ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config

  module Make (I : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) :
    S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type commit_key = Store.commit_key
       and type contents_key = Store.contents_key
       and type node_key = Store.node_key
end
