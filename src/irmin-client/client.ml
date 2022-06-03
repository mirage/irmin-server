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

  let tls = Irmin.Backend.Conf.key ~spec "tls" Irmin.Type.bool false

  let hostname =
    Irmin.Backend.Conf.key ~spec "hostname" Irmin.Type.string "127.0.0.1"
end

let config ?(tls = false) ?hostname uri =
  let default_host = Uri.host_with_default ~default:"127.0.0.1" uri in
  let config =
    Irmin.Backend.Conf.add (Irmin.Backend.Conf.empty Conf.spec) Conf.uri uri
  in
  let config =
    Irmin.Backend.Conf.add config Conf.hostname
      (Option.value ~default:default_host hostname)
  in
  Irmin.Backend.Conf.add config Conf.tls tls

module Client (I : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module C = Command.Make (I) (Codec) (Store)
  open C
  module IO = I

  type conf = { uri : Uri.t; tls : bool; hostname : string; ctx : IO.ctx }

  type t = {
    conf : conf;
    mutable conn : Conn.t;
    mutable closed : bool;
    lock : Lwt_mutex.t;
  }

  (*let uri t = t.conf.uri*)

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

  let lock t f = Lwt_mutex.with_lock t.lock f [@@inline]

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

  let recv_branch_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn
      (Irmin.Type.pair Store.Branch.t (Irmin.Diff.t Store.commit_key_t))
    >|= Error.unwrap "recv_branch_diff"

  let recv_branch_key_diff (t : t) =
    let* _status = Conn.Response.read_header t.conn in
    Conn.read t.conn (Irmin.Diff.t Store.commit_key_t)
    >|= Error.unwrap "recv_branch_key_diff"

  (*module Cache = struct
      module Contents = Irmin.Backend.Lru.Make (struct
        type t = Store.Hash.t

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal Store.Hash.t))
      end)

      module Commit = Irmin.Backend.Lru.Make (struct
        type t = Store.commit_key

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal Store.commit_key_t))
      end)

      let commit : Store.commit Commit.t = Commit.create 64
      let contents : Store.contents Contents.t = Contents.create 64
    end*)

  (*module Contents = struct
      include Store.Contents

      type key = Store.contents_key

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
    end*)

  (*
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
    end*)
end

module Make (IO : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) =
struct
  module St = Store
  module Client = Client (IO) (Codec) (Store)
  module Command = Command.Make (IO) (Codec) (Store)
  module Conn = Command.Conn
  module Commands = Command.Commands

  let request = Client.request

  let rec connect conf =
    let client = Client.mk_client conf in
    let* ic, oc = IO.connect ~ctx:conf.ctx client in
    let conn = Conn.v ic oc in
    let+ ok = Conn.Handshake.V1.send (module Store) conn in
    if not ok then Error.raise_error "invalid handshake"
    else
      let t =
        Client.{ conf; conn; closed = false; lock = Lwt_mutex.create () }
      in
      t

  and reconnect t =
    let* () = Lwt.catch (fun () -> Client.close t) (fun _ -> Lwt.return_unit) in
    let+ conn = connect t.Client.conf in
    t.conn <- conn.conn;
    t.closed <- false

  let dup client =
    let* c = connect client.Client.conf in
    if client.closed then
      let () = c.closed <- true in
      Lwt.return c
    else
      let* branch =
        request client (module Commands.Get_current_branch) ()
        >|= Error.unwrap "dup:get_current_branch"
      in
      let* _ = request c (module Commands.Set_current_branch) branch in
      Lwt.return c

  let uri t = t.Client.conf.uri

  module X = struct
    open Lwt.Infix
    module Schema = St.Schema
    module Hash = St.Hash

    module Contents = struct
      type nonrec 'a t = Client.t

      open Commands.Contents
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

      let batch t f = f t
      let close t = Client.close t

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
      type nonrec 'a t = Client.t

      open Commands.Node
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

      let batch t f = f t
      let close t = Client.close t

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
      type nonrec 'a t = Client.t

      open Commands.Commit
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

      let batch t f = f t
      let close t = Client.close t

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
      type nonrec t = Client.t

      open Commands.Branch
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
                  (fun () -> Client.recv_branch_diff t)
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
                  (fun () -> Client.recv_branch_key_diff t)
                  (fun _ -> raise Continue)
                >>= f >>= loop)
              (function _ -> loop ())
        in
        Lwt.async loop;
        Lwt.return t

      let unwatch _t watch =
        let* () = Conn.write watch.Client.conn Unwatch.req_t () in
        Client.close watch

      let clear t = request t (module Clear) () >|= Error.unwrap "Branch.clear"
      let close t = Client.close t
    end

    module Slice = Store.Backend.Slice

    module Repo = struct
      type nonrec t = Client.t

      let v config =
        let uri = Irmin.Backend.Conf.get config Conf.uri in
        let tls = Irmin.Backend.Conf.get config Conf.tls in
        let hostname = Irmin.Backend.Conf.get config Conf.hostname in
        let conf =
          Client.{ uri; tls; hostname; ctx = Lazy.force IO.default_ctx }
        in
        connect conf

      let close (t : t) = Client.close t
      let contents_t (t : t) = t
      let node_t (t : t) = t
      let commit_t (t : t) = t
      let branch_t (t : t) = t
      let batch (t : t) f = f t t t
    end

    module Remote = Irmin.Backend.Remote.None (Commit.Key) (Store.Branch)
  end

  include Irmin.Of_backend (X)

  let ping t = request t (module Commands.Ping) ()
  let export ?depth t = request t (module Commands.Export) depth
  let import t slice = request t (module Commands.Import) slice
  let close t = Client.close t

  let connect ?tls ?hostname uri =
    let conf = config ?tls ?hostname uri in
    Repo.v conf

  let current_branch (t : t) =
    request (repo t) (module Commands.Get_current_branch) ()

  module Batch = struct
    module Tree' = Tree

    type store = t

    module Tree = struct
      include Command.Tree

      let empty (t : repo) = request t (module Commands.Tree.Empty) ()

      let of_path t path =
        let t = repo t in
        request t (module Commands.Tree.Of_path) path

      let of_hash (t : repo) hash =
        request t (module Commands.Tree.Of_hash) hash

      let of_commit (t : repo) hash =
        request t (module Commands.Tree.Of_commit) hash

      let of_key key = Key key

      let to_local t tree =
        let+ res = request t (module Commands.Tree.To_local) tree in
        match res with
        | Ok x ->
            let x = Tree.of_concrete x in
            Ok (x : tree)
        | Error e -> Error e

      let of_local x =
        let+ x = Tree.to_concrete x in
        Concrete x

      let save t tree = request t (module Commands.Tree.Save) tree
      let key t tree = request t (module Commands.Tree.Key) tree
      let hash t tree = request t (module Commands.Tree.Hash) tree

      let add t tree path value =
        request t (module Commands.Tree.Add) (tree, path, value)

      let add_tree t tree path tree' =
        request t (module Commands.Tree.Add_tree) (tree, path, tree')

      let find t tree path : contents option Error.result Lwt.t =
        request t (module Commands.Tree.Find) (tree, path)

      let find_tree t tree path : t option Error.result Lwt.t =
        request t (module Commands.Tree.Find_tree) (tree, path)

      let remove t tree path =
        request t (module Commands.Tree.Remove) (tree, path)

      let cleanup t tree = request t (module Commands.Tree.Cleanup) tree
      let cleanup_all t = request t (module Commands.Tree.Cleanup_all) ()
      let mem t tree path = request t (module Commands.Tree.Mem) (tree, path)

      let mem_tree t tree path =
        request t (module Commands.Tree.Mem_tree) (tree, path)

      let list t tree path = request t (module Commands.Tree.List) (tree, path)
      let merge t ~old a b = request t (module Commands.Tree.Merge) (old, a, b)
    end

    type batch_contents =
      [ `Hash of Store.Hash.t | `Value of Store.contents ]
      * Store.metadata option

    type t =
      (Store.path * [ `Contents of batch_contents | `Tree of Tree.t ] option)
      list
    [@@deriving irmin]

    let build_tree (t : repo) (batch : t) tree =
      request t (module Commands.Tree.Batch_build_tree) (tree, batch)

    let apply ~info t path (batch : t) =
      let t = repo t in
      request t (module Commands.Tree.Batch_apply) (path, info (), batch)

    let path_equal = Irmin.Type.(unstage (equal Path.t))

    let find (b : t) k =
      let l =
        List.filter_map
          (fun (a, b) ->
            match b with
            | Some (`Contents x) when path_equal k a -> Some x
            | _ -> None)
          b
      in
      match l with [] -> None | h :: _ -> Some h

    let find_tree (b : t) k =
      let l =
        List.filter_map
          (fun (a, b) ->
            match b with
            | Some (`Tree x) when path_equal k a -> Some x
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

    let add_tree (batch : t) (path : path) (tree : Tree.t) : t =
      (path, Some (`Tree tree)) :: batch
  end

  (* Overrides *)

  module Commit = struct
    include Commit

    module Cache = struct
      module Key = Irmin.Backend.Lru.Make (struct
        type t = commit_key

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal commit_key_t))
      end)

      module Hash = Irmin.Backend.Lru.Make (struct
        type t = hash

        let hash = Hashtbl.hash
        let equal = Irmin.Type.(unstage (equal hash_t))
      end)

      let key : commit Key.t = Key.create 32
      let hash : commit Hash.t = Hash.create 32
    end

    let of_key repo key =
      if Cache.Key.mem Cache.key key then
        Lwt.return_some (Cache.Key.find Cache.key key)
      else
        let+ x = of_key repo key in
        Option.iter (Cache.Key.add Cache.key key) x;
        x

    let of_hash repo hash =
      if Cache.Hash.mem Cache.hash hash then
        Lwt.return_some (Cache.Hash.find Cache.hash hash)
      else
        let+ x = of_hash repo hash in
        Option.iter (Cache.Hash.add Cache.hash hash) x;
        x
  end

  let main repo =
    let* () =
      request repo (module Commands.Set_current_branch) Store.Branch.main
      >|= Error.unwrap "Store.main"
    in
    main repo

  let of_branch repo branch =
    let* () =
      request repo (module Commands.Set_current_branch) branch
      >|= Error.unwrap "Store.of_branch"
    in
    of_branch repo branch

  let clone ~src ~dst =
    let repo = repo src in
    let* repo = dup repo in
    let* () =
      Head.find src >>= function
      | None -> Branch.remove repo dst
      | Some h -> Branch.set repo dst h
    in
    of_branch repo dst

  let mem store path =
    let repo = repo store in
    request repo (module Commands.Store.Mem) path >|= Error.unwrap "mem"

  let mem_tree store path =
    let repo = repo store in
    request repo (module Commands.Store.Mem_tree) path
    >|= Error.unwrap "mem_tree"

  let find store path =
    let repo = repo store in
    request repo (module Commands.Store.Find) path >|= Error.unwrap "find"

  let remove_exn ?retries ?allow_empty ?parents ~info store path =
    let parents = Option.map (List.map (fun c -> Commit.hash c)) parents in
    let repo = repo store in
    request repo
      (module Commands.Store.Remove)
      ((retries, allow_empty, parents), path, info ())
    >|= Error.unwrap "remove"

  let remove ?retries ?allow_empty ?parents ~info store path =
    let* x = remove_exn ?retries ?allow_empty ?parents ~info store path in
    Lwt.return_ok x

  let get store path =
    let* x = find store path in
    match x with
    | None ->
        invalid_arg ("Contents not found: " ^ Irmin.Type.to_string path_t path)
    | Some x -> Lwt.return x

  let find_tree store path =
    let repo = repo store in
    let+ concrete =
      request repo (module Commands.Store.Find_tree) path
      >|= Error.unwrap "find_tree"
    in
    Option.map Tree.of_concrete concrete
end
