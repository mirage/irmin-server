open Irmin_server_internal
open Lwt.Syntax
open Lwt.Infix
include Client_intf

exception Continue

module Conf = struct
  let spec = Irmin.Backend.Conf.Spec.v "irmin-client"
  let uri = Irmin.Type.(map string) Uri.of_string Uri.to_string

  let uri =
    Irmin.Backend.Conf.key ~spec "uri" uri
      (Uri.of_string "tcp://127.0.0.1:9181")

  let batch_size = Irmin.Backend.Conf.key ~spec "client" Irmin.Type.int 32
  let tls = Irmin.Backend.Conf.key ~spec "tls" Irmin.Type.bool false

  let hostname =
    Irmin.Backend.Conf.key ~spec "hostname" Irmin.Type.string "127.0.0.1"
end

let config ?(batch_size = 32) ?(tls = false) ?hostname uri =
  let default_host = Uri.host_with_default ~default:"127.0.0.1" uri in
  let config =
    Irmin.Backend.Conf.add (Irmin.Backend.Conf.empty Conf.spec) Conf.uri uri
  in
  let config = Irmin.Backend.Conf.add config Conf.batch_size batch_size in
  let config =
    Irmin.Backend.Conf.add config Conf.hostname
      (Option.value ~default:default_host hostname)
  in
  Irmin.Backend.Conf.add config Conf.tls tls

module Make (I : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module C = Command.Make (I) (Codec) (Store)
  module St = Store
  open C
  module Hash = Store.Hash
  module Path = Store.Path
  module Metadata = Store.Metadata
  module Info = Store.Info
  module IO = I

  module Private = struct
    module Store = C.Store
    module Tree = C.Tree
  end

  module Schema = C.Store.Schema

  type hash = Store.hash
  type contents = Store.contents
  type branch = Store.branch
  type path = Store.path
  type step = Store.step
  type commit = C.Commit.t
  type slice = St.slice
  type metadata = St.metadata
  type contents_key = Store.contents_key

  let slice_t = St.slice_t

  type conf = {
    uri : Uri.t;
    tls : bool;
    hostname : string;
    batch_size : int;
    ctx : IO.ctx;
  }

  type t = {
    conf : conf;
    mutable conn : Conn.t;
    mutable closed : bool;
    lock : Lwt_mutex.t;
  }

  let uri t = t.conf.uri

  type batch =
    (Store.path
    * [ `Contents of
        [ `Hash of Store.Hash.t | `Value of Store.contents ]
        * Store.metadata option
      | `Tree of Tree.t ]
      option)
    list
  [@@deriving irmin]

  type tree = t * Private.Tree.t * batch

  let close t =
    t.closed <- true;
    IO.close (t.conn.ic, t.conn.oc)

  let mk_client { uri; tls; hostname; _ } =
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
    let client =
      match String.lowercase_ascii scheme with
      | "unix" -> `Unix_domain_socket (`File (Uri.path uri))
      | "tcp" ->
          let port = Uri.port uri |> Option.value ~default:9181 in
          let ip = Ipaddr.of_string_exn addr in
          if not tls then `TCP (`IP ip, `Port port)
          else `TLS (`Hostname hostname, `IP ip, `Port port)
      | "ws" | "wss" -> (
          let port = Uri.port uri |> Option.value ~default:9181 in
          match Ipaddr.of_string addr with
          | Ok ip ->
              if not tls then `Ws (Some (`IP ip, `Port port), Uri.to_string uri)
              else `TLS (`Hostname hostname, `IP ip, `Port port)
          | _ -> `Ws (None, Uri.to_string uri))
      | x -> invalid_arg ("Unknown client scheme: " ^ x)
    in
    client

  let rec connect conf =
    let client = mk_client conf in
    let* ic, oc = IO.connect ~ctx:conf.ctx client in
    let conn = Conn.v ic oc in
    let+ ok = Conn.Handshake.V1.send (module Private.Store) conn in
    if not ok then Error.raise_error "invalid handshake"
    else
      let t = { conf; conn; closed = false; lock = Lwt_mutex.create () } in
      t

  and reconnect t =
    let* () = Lwt.catch (fun () -> close t) (fun _ -> Lwt.return_unit) in
    let+ conn = connect t.conf in
    t.conn <- conn.conn;
    t.closed <- false

  let lock t f = Lwt_mutex.with_lock t.lock f [@@inline]

  let dup client =
    let* c = connect client.conf in
    if client.closed then c.closed <- true;
    Lwt.return c

  let send_command_header t (module Cmd : C.CMD) =
    let header = Conn.Request.v_header ~command:Cmd.name in
    Conn.Request.write_header t.conn header

  let recv (t : t) name ty =
    let* res = Conn.Response.read_header t.conn in
    Conn.Response.get_error t.conn res >>= function
    | Some err ->
        Logs.err (fun l -> l "Request error: command=%s, error=%s" name err);
        Lwt.return_error (`Msg err)
    | None ->
        let+ x = Conn.read t.conn ty in
        Logs.debug (fun l -> l "Completed request: command=%s" name);
        x

  let request (t : t) (type x y)
      (module Cmd : C.CMD with type res = x and type req = y) (a : y) =
    if t.closed then raise Irmin.Closed
    else
      let name = Cmd.name in
      Logs.debug (fun l -> l "Starting request: command=%s" name);
      lock t (fun () ->
          let* () = send_command_header t (module Cmd) in
          let* () = Conn.write t.conn Cmd.req_t a in
          let* () = IO.flush t.conn.oc in
          recv t name Cmd.res_t)

  let recv_commit_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn (Irmin.Diff.t Commit.t) >|= Error.unwrap "recv_commit_diff"

  let recv_branch_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn
      (Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t))
    >|= Error.unwrap "recv_branch_diff"

  let recv_branch_key_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn (Irmin.Diff.t Store.commit_key_t)
    >|= Error.unwrap "recv_branch_key_diff"

  module Cache = struct
    module Contents = Irmin.Backend.Lru.Make (struct
      type t = St.Hash.t

      let hash = Hashtbl.hash
      let equal = Irmin.Type.(unstage (equal St.Hash.t))
    end)

    module Commit = Irmin.Backend.Lru.Make (struct
      type t = St.commit_key

      let hash = Hashtbl.hash
      let equal = Irmin.Type.(unstage (equal St.commit_key_t))
    end)

    let commit : commit Commit.t = Commit.create 64
    let contents : contents Contents.t = Contents.create 64
  end

  let ping t = request t (module Commands.Ping) ()
  let export ?depth t = request t (module Commands.Export) depth
  let import t slice = request t (module Commands.Import) slice

  type watch = t

  let unwatch t =
    let* () = Conn.write t.conn Commands.Unwatch.req_t () in
    let+ () = close t in
    Ok ()

  let watch f t =
    let* t = dup t in
    request t (module Commands.Watch) () >>= function
    | Error e -> Lwt.return_error e
    | Ok () ->
        let rec loop () =
          Lwt.catch
            (fun () ->
              Lwt.catch (fun () -> recv_commit_diff t) (fun _ -> raise Continue)
              >>= f >>= loop)
            (function _ -> loop ())
        in
        Lwt.async loop;
        Lwt.return_ok t

  module Branch = struct
    include Store.Branch

    let set_current t (branch : Store.branch) =
      request t (module Commands.Set_current_branch) branch

    let get_current t = request t (module Commands.Get_current_branch) ()
    let get ?branch t = request t (module Commands.Branch_head) branch

    let set ?branch t commit =
      request t (module Commands.Branch_set_head) (branch, commit)

    let remove t branch = request t (module Commands.Branch_remove) branch
  end

  module Contents = struct
    include St.Contents

    type key = contents_key

    let of_hash t hash =
      if Cache.Contents.mem Cache.contents hash then
        Lwt.return_ok (Some (Cache.Contents.find Cache.contents hash))
      else request t (module Commands.Contents_of_hash) hash

    let exists' t contents =
      let hash = hash contents in
      if Cache.Contents.mem Cache.contents hash then Lwt.return_ok (hash, true)
      else
        let* res = request t (module Commands.Contents_exists) hash in
        match res with
        | Ok true ->
            Cache.Contents.add Cache.contents hash contents;
            Lwt.return_ok (hash, true)
        | x -> Lwt.return (Result.map (fun y -> (hash, y)) x)

    let exists t contents = exists' t contents >|= Result.map snd
    let save t contents = request t (module Commands.Contents_save) contents
  end

  module Tree = struct
    type store = t
    type key = St.Tree.kinded_key

    let key_t = St.Tree.kinded_key_t

    let rec build (t : store) ?tree b : tree Error.result Lwt.t =
      let tree =
        match tree with
        | Some tree -> tree
        | None ->
            let _, tree, _ = empty t in
            tree
      in
      match b with
      | [] -> Lwt.return_ok (t, tree, [])
      | b -> batch_update (t, tree, b) []

    and batch_update (((t : store), tree, batch) : tree) l =
      wrap t
        (request t
           (module Commands.Tree.Batch_update)
           (tree, List.rev_append batch (List.rev l)))

    and wrap ?(batch = []) store tree =
      tree >>= fun tree ->
      Lwt.return (Result.map (fun tree -> (store, tree, batch)) tree)

    and empty (t : store) : tree = (t, Tree.Local (`Tree []), [])

    module Batch = struct
      let path_equal = Irmin.Type.(unstage (equal Path.t))

      let find b k =
        let l =
          List.filter_map
            (fun (a, b) ->
              match b with
              | Some (`Contents _ as x) when path_equal k a -> Some x
              | _ -> None)
            b
        in
        match l with [] -> None | h :: _ -> Some h

      let find_tree b k =
        let l =
          List.filter_map
            (fun (a, b) ->
              match b with
              | Some (`Tree _ as x) when path_equal k a -> Some x
              | _ -> None)
            b
        in
        match l with [] -> None | h :: _ -> Some h

      let mem b k =
        List.exists
          (fun (a, b) ->
            match b with Some (`Contents _) -> path_equal k a | _ -> false)
          b

      let mem_tree b k =
        List.exists
          (fun (a, b) ->
            match b with Some (`Tree _) -> path_equal k a | _ -> false)
          b

      let remove b k = (k, None) :: b

      let add batch path ?metadata value =
        (path, Some (`Contents (`Value value, metadata))) :: batch

      let add_hash batch path ?metadata hash =
        (path, Some (`Contents (`Hash hash, metadata))) :: batch

      let add_tree batch path (_, tree, batch') =
        ((path, Some (`Tree tree)) :: batch') @ batch
    end

    let split t = t
    let v t ?(batch = []) tr = (t, tr, batch)
    let of_key t k = (t, Private.Tree.Key k, [])

    let map_tree tree f =
      Result.map (fun (_, tree, _) -> f tree) tree |> function
      | Ok x -> (
          x >>= function
          | Ok x -> Lwt.return_ok x
          | Error e -> Lwt.return_error e)
      | Error e -> Lwt.return_error e

    let clear (t, tree, batch) =
      let* tree = build t ~tree batch in
      map_tree tree (fun tree -> request t (module Commands.Tree.Clear) tree)

    let key (t, tree, batch) =
      let* tree = build t ~tree batch in
      map_tree tree (fun tree -> request t (module Commands.Tree.Key) tree)

    let add' (t, tree, batch) path value =
      wrap ~batch t (request t (module Commands.Tree.Add) (tree, path, value))

    let add ((t, tree, batch) : tree) path ?metadata value =
      let hash = St.Contents.hash value in
      let exists = Cache.Contents.mem Cache.contents hash in
      let batch =
        if exists then Batch.add_hash batch ?metadata path hash
        else Batch.add batch ?metadata path value
      in
      if List.length batch > t.conf.batch_size then build t ~tree batch
      else Lwt.return_ok (t, tree, batch)

    let add_tree ((t, tree, batch) : tree) path tr =
      let batch = Batch.add_tree batch path tr in
      if List.length batch > t.conf.batch_size then build t ~tree batch
      else Lwt.return_ok (t, tree, batch)

    let add_tree' (t, tree, batch) path (_, tr, batch') =
      wrap ~batch:(batch @ batch') t
        (request t (module Commands.Tree.Add_tree) (tree, path, tr))

    let find ((t, tree, batch) : tree) path : contents option Error.result Lwt.t
        =
      let x = Batch.find batch path in
      match x with
      | Some (`Contents (`Value x, _)) -> Lwt.return_ok (Some x)
      | Some (`Contents (`Hash x, _)) -> Contents.of_hash t x
      | _ -> request t (module Commands.Tree.Find) (tree, path)

    let find_tree ((t, tree, batch) : tree) path :
        tree option Error.result Lwt.t =
      let x = Batch.find_tree batch path in
      match x with
      | Some (`Tree x) -> Lwt.return_ok (Some (t, x, []))
      | _ ->
          let+ tree = request t (module Commands.Tree.Find_tree) (tree, path) in
          Result.map (Option.map (fun tree -> (t, tree, []))) tree

    let remove (t, tree, batch) path =
      let batch = Batch.remove batch path in
      wrap ~batch t (request t (module Commands.Tree.Remove) (tree, path))

    let cleanup (t, tree, _) = request t (module Commands.Tree.Cleanup) tree

    let mem (t, tree, batch) path =
      if Batch.mem batch path then Lwt.return_ok true
      else request t (module Commands.Tree.Mem) (tree, path)

    let mem_tree (t, tree, batch) path =
      if Batch.mem_tree batch path then Lwt.return_ok true
      else request t (module Commands.Tree.Mem_tree) (tree, path)

    let list (t, tree, batch) path =
      let* tree = build t ~tree batch in
      map_tree tree (fun tree ->
          request t (module Commands.Tree.List) (tree, path))

    let merge ~old:(_, old, old') (t, a, a') (_, b, b') =
      let* _, old, _ = build t ~tree:old old' >|= Error.unwrap "build:old" in
      let* _, a, _ = build t ~tree:a a' >|= Error.unwrap "build:a" in
      let* _, b, _ = build t ~tree:b b' >|= Error.unwrap "build:b" in
      wrap t (request t (module Commands.Tree.Merge) (old, a, b))

    module Local = Private.Tree.Local

    let to_local (t, tree, batch) =
      let* tree = build t ~tree batch in
      match tree with
      | Error e -> Lwt.return_error e
      | Ok (_, tree, _) -> (
          let+ res = request t (module Commands.Tree.To_local) tree in
          match res with
          | Ok x ->
              let x = Private.Tree.Local.of_concrete x in
              Ok (x : Private.Store.tree)
          | Error e -> Error e)

    let of_local t x =
      let+ x = Private.Tree.Local.to_concrete x in
      (t, Private.Tree.Local x, [])

    let save (t, tree, batch) =
      let* tree = build t ~tree batch in
      match tree with
      | Error e -> Lwt.return_error e
      | Ok (_, tree, _) -> request t (module Commands.Tree.Save) tree

    let hash (t, tree, batch) =
      let* tree = build t ~tree batch in
      match tree with
      | Error e -> Lwt.return_error e
      | Ok (_, tree, _) -> request t (module Commands.Tree.Hash) tree

    let cleanup_all t = request t (module Commands.Tree.Cleanup_all) ()

    type t = tree
  end

  let find t path = request t (module Commands.Store.Find) path

  let set t ~info path value =
    request t (module Commands.Store.Set) (path, info (), value)

  let test_and_set t ~info path ~test ~set =
    request t (module Commands.Store.Test_and_set) (path, info (), (test, set))

  let remove t ~info path =
    request t (module Commands.Store.Remove) (path, info ())

  let find_tree t path =
    let+ tree = request t (module Commands.Store.Find_tree) path in
    Result.map (fun x -> Option.map (fun x -> (t, x, [])) x) tree

  let set_tree t ~info path (_, tree, batch) =
    let* tree = Tree.build t ~tree batch in
    match tree with
    | Error e -> Lwt.return_error e
    | Ok (_, tree, _) ->
        let+ tree =
          request t (module Commands.Store.Set_tree) (path, info (), tree)
        in
        Result.map (fun tree -> (t, tree, [])) tree

  let test_and_set_tree t ~info path ~test ~set =
    let test = Option.map (fun (_, x, _) -> x) test in
    let set = Option.map (fun (_, x, _) -> x) set in
    let+ tree =
      request t
        (module Commands.Store.Test_and_set_tree)
        (path, info (), (test, set))
    in
    Result.map (Option.map (fun tree -> (t, tree, []))) tree

  let mem t path = request t (module Commands.Store.Mem) path
  let mem_tree t path = request t (module Commands.Store.Mem_tree) path

  let merge t ~info branch =
    request t (module Commands.Store.Merge) (info (), branch)

  let merge_commit t ~info commit =
    request t (module Commands.Store.Merge_commit) (info (), commit)

  let last_modified t path =
    request t (module Commands.Store.Last_modified) path

  module Commit = struct
    include C.Commit

    let v t ~info ~parents ((_, tree, batch) : Tree.t) : t Error.result Lwt.t =
      let* tree = Tree.build t ~tree batch in
      match tree with
      | Error e -> Lwt.return_error e
      | Ok (_, tree, _) -> (
          request t (module Commands.Commit_v) (info (), parents, tree)
          >|= function
          | Error e -> Error e
          | Ok (x : t) ->
              let key = Commit.key x in
              Cache.Commit.add Cache.commit key x;
              Ok x)

    let of_key t key =
      if Cache.(Commit.mem commit key) then
        Lwt.return_ok (Some Cache.(Commit.find commit key))
      else
        let* commit = request t (module Commands.Commit_of_key) key in
        match commit with
        | Ok c ->
            Option.iter (Cache.Commit.add Cache.commit key) c;
            Lwt.return_ok c
        | Error e -> Lwt.return_error e

    let of_hash t hash = request t (module Commands.Commit_of_hash) hash

    let hash t commit =
      request t (module Commands.Commit_hash_of_key) (key commit)

    let tree t commit = (t, tree commit, [])
  end
end

module Make_store (I : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module St = Store
  module Client = Make (I) (Codec) (Store)
  module Command = Command.Make (I) (Codec) (Store)
  open Command
  open Client

  module X = struct
    open Lwt.Infix
    module Schema = St.Schema
    module Hash = St.Hash

    module Contents = struct
      type nonrec 'a t = t

      open Commands.Backend.Contents
      module Key = St.Backend.Contents.Key
      module Val = St.Backend.Contents.Val
      module Hash = St.Backend.Contents.Hash

      type key = Key.t
      type value = Val.t
      type hash = Hash.t

      let mem t key = request t (module Mem) key >|= Error.unwrap "Contents.mem"

      let find t key =
        request t (module Find) key >|= Error.unwrap "Contents.find"

      let add t value =
        request t (module Add) value >|= Error.unwrap "Contents.add"

      let unsafe_add t key value =
        request t (module Unsafe_add) (key, value)
        >|= Error.unwrap "Contents.unsafe_add"

      let index t hash =
        request t (module Index) hash >|= Error.unwrap "Contents.index"

      let clear t =
        request t (module Clear) () >|= Error.unwrap "Contents.clear"

      let batch t f = f t
      let close t = close t

      let merge t =
        let f ~old a b =
          let* old = old () in
          match old with
          | Ok old ->
              request t (module Merge) (old, a, b)
              >|= Error.unwrap "Contents.merge"
          | Error e -> Lwt.return_error e
        in
        Irmin.Merge.v Irmin.Type.(option Key.t) f
    end

    module Node = struct
      type nonrec 'a t = t

      open Commands.Backend.Node
      module Key = St.Backend.Node.Key
      module Val = St.Backend.Node.Val
      module Hash = St.Backend.Node.Hash
      module Path = St.Backend.Node.Path
      module Metadata = St.Backend.Node.Metadata
      module Contents = St.Backend.Node.Contents

      type key = Key.t
      type value = Val.t
      type hash = Hash.t

      let mem t key = request t (module Mem) key >|= Error.unwrap "Node.mem"
      let find t key = request t (module Find) key >|= Error.unwrap "Node.find"
      let add t value = request t (module Add) value >|= Error.unwrap "Node.add"

      let unsafe_add t key value =
        request t (module Unsafe_add) (key, value)
        >|= Error.unwrap "Node.unsafe_add"

      let index t hash =
        request t (module Index) hash >|= Error.unwrap "Node.index"

      let clear t = request t (module Clear) () >|= Error.unwrap "Node.clear"
      let batch t f = f t
      let close t = close t

      let merge t =
        let f ~old a b =
          let* old = old () in
          match old with
          | Ok old ->
              request t (module Merge) (old, a, b) >|= Error.unwrap "Node.merge"
          | Error e -> Lwt.return_error e
        in
        Irmin.Merge.v Irmin.Type.(option Key.t) f
    end

    module Node_portable = St.Backend.Node_portable

    module Commit = struct
      type nonrec 'a t = t

      open Commands.Backend.Commit
      module Key = St.Backend.Commit.Key
      module Val = St.Backend.Commit.Val
      module Hash = St.Backend.Commit.Hash
      module Info = St.Backend.Commit.Info
      module Node = Node

      type key = Key.t
      type value = Val.t
      type hash = Hash.t

      let mem t key = request t (module Mem) key >|= Error.unwrap "Commit.mem"

      let find t key =
        request t (module Find) key >|= Error.unwrap "Commit.find"

      let add t value =
        request t (module Add) value >|= Error.unwrap "Commit.add"

      let unsafe_add t key value =
        request t (module Unsafe_add) (key, value)
        >|= Error.unwrap "Commit.unsafe_add"

      let index t hash =
        request t (module Index) hash >|= Error.unwrap "Commit.index"

      let clear t = request t (module Clear) () >|= Error.unwrap "Commit.clear"
      let batch t f = f t
      let close t = close t

      let merge t ~info =
        let f ~old a b =
          let* old = old () in
          match old with
          | Ok old ->
              request t (module Merge) (info (), (old, a, b))
              >|= Error.unwrap "Node.merge"
          | Error e -> Lwt.return_error e
        in
        Irmin.Merge.v Irmin.Type.(option Key.t) f
    end

    module Commit_portable = St.Backend.Commit_portable

    module Branch = struct
      type nonrec t = t

      open Commands.Backend.Branch
      module Key = St.Backend.Branch.Key
      module Val = St.Backend.Branch.Val

      type key = Key.t
      type value = Val.t

      let mem t key = request t (module Mem) key >|= Error.unwrap "Branch.mem"

      let find t key =
        request t (module Find) key >|= Error.unwrap "Branch.find"

      let set t key value =
        request t (module Set) (key, value) >|= Error.unwrap "Branch.set"

      let test_and_set t key ~test ~set =
        request t (module Test_and_set) (key, test, set)
        >|= Error.unwrap "Branch.test_and_set"

      let remove t key =
        request t (module Remove) key >|= Error.unwrap "Branch.remove"

      let list t = request t (module List) () >|= Error.unwrap "Branch.list"

      type watch = t

      let watch t ?init f =
        let* t = dup t in
        let* () =
          request t (module Watch) init >|= Error.unwrap "Branch.watch"
        in
        let rec loop () =
          if t.closed || Conn.is_closed t.conn then Lwt.return_unit
          else
            Lwt.catch
              (fun () ->
                Lwt.catch
                  (fun () -> recv_branch_diff t)
                  (fun _ -> raise Continue)
                >>= fun (key, diff) -> f key diff >>= loop)
              (function _ -> loop ())
        in
        Lwt.async loop;
        Lwt.return t

      let watch_key t key ?init f =
        let* t = dup t in
        let* () =
          request t (module Watch_key) (init, key)
          >|= Error.unwrap "Branch.watch_key"
        in
        let rec loop () =
          if t.closed || Conn.is_closed t.conn then Lwt.return_unit
          else
            Lwt.catch
              (fun () ->
                Lwt.catch
                  (fun () -> recv_branch_key_diff t)
                  (fun _ -> raise Continue)
                >>= f >>= loop)
              (function _ -> loop ())
        in
        Lwt.async loop;
        Lwt.return t

      let unwatch _t watch =
        let* () = Conn.write watch.conn Unwatch.req_t () in
        close watch

      let clear t = request t (module Clear) () >|= Error.unwrap "Branch.clear"
      let close t = close t
    end

    module Slice = Irmin.Backend.Slice.Make (Contents) (Node) (Commit)

    module Repo = struct
      type nonrec t = t

      let v config =
        let uri = Irmin.Backend.Conf.get config Conf.uri in
        let tls = Irmin.Backend.Conf.get config Conf.tls in
        let batch_size = Irmin.Backend.Conf.get config Conf.batch_size in
        let hostname = Irmin.Backend.Conf.get config Conf.hostname in
        let conf =
          { uri; tls; batch_size; hostname; ctx = Lazy.force IO.default_ctx }
        in
        connect conf

      let close (t : t) = close t
      let contents_t (t : t) = t
      let node_t (t : t) = t
      let commit_t (t : t) = t
      let branch_t (t : t) = t
      let batch (t : t) f = f t t t
    end

    module Remote = Irmin.Backend.Remote.None (Commit.Key) (Store.Branch)
  end

  include Irmin.Of_backend (X)

  let to_backend_node node = try to_backend_node node with exc -> raise exc
  (* TODO: figure out how to handle this? *)
end
