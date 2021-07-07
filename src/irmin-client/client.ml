open Irmin_server_types
open Lwt.Syntax
open Lwt.Infix
include Client_intf

module Make (C : Command.S) = struct
  module St = C.Store
  open C
  module Hash = Store.Hash
  module Key = Store.Key
  module Metadata = Store.Metadata

  module Info = struct
    include Irmin_unix.Info (Store.Info)

    let init = Store.Info.v
  end

  module Private = struct
    module Store = C.Store
    module Tree = C.Tree
  end

  module Schema = C.Store.Schema

  type hash = Store.hash

  type contents = Store.contents

  type branch = Store.branch

  type key = Store.key

  type step = Store.step

  type commit = C.Commit.t

  type slice = St.slice

  type stats = Stats.t

  type metadata = St.metadata

  let stats_t = Stats.t

  let slice_t = St.slice_t

  type conf = {
    uri : Uri.t;
    client : Conduit_lwt_unix.client;
    batch_size : int;
  }

  type t = { conf : conf; mutable conn : Conn.t }

  let uri t = t.conf.uri

  type batch =
    (key
    * [ `Contents of
        [ `Hash of Tree.Private.Store.hash
        | `Value of Tree.Private.Store.contents ]
      | `Tree of Tree.t ])
    list

  type tree = t * Private.Tree.t * batch

  let conf ?(batch_size = 32) ?(tls = false) ~uri () =
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
    let client =
      match String.lowercase_ascii scheme with
      | "unix" -> `Unix_domain_socket (`File (Uri.path uri))
      | "tcp" ->
          let ip = Unix.gethostbyname addr in
          let port = Uri.port uri |> Option.value ~default:9181 in
          let ip =
            ip.h_addr_list.(0) |> Unix.string_of_inet_addr
            |> Ipaddr.of_string_exn
          in
          if not tls then `TCP (`IP ip, `Port port)
          else `TLS (`Hostname addr, `IP ip, `Port port)
      | x -> invalid_arg ("Unknown client scheme: " ^ x)
    in
    { client; batch_size; uri }

  let connect' conf =
    let ctx = Conduit_lwt_unix.default_ctx in
    let* flow, ic, oc = Conduit_lwt_unix.connect ~ctx conf.client in
    let conn = Conn.v flow ic oc in
    let+ () = Handshake.V1.send (module Private.Store) ic oc in
    { conf; conn }

  let connect ?batch_size ?tls ~uri () =
    let client = conf ?batch_size ?tls ~uri () in
    connect' client

  let dup client = connect' client.conf

  let close t = Conduit_lwt_server.close (t.conn.ic, t.conn.oc)

  let handle_disconnect t f =
    Lwt.catch f (function
      | End_of_file ->
          Logs.info (fun l -> l "Reconnecting to server");
          let* conn = connect' t.conf in
          t.conn <- conn.conn;
          f ()
      | exn -> raise exn)
    [@@inline]

  let send_command_header t (module Cmd : C.CMD) =
    let header = Request.Header.v ~command:Cmd.name in
    Request.Write.header t.conn.Conn.oc header

  let recv (t : t) name ty =
    let* res = Response.Read.header t.conn.ic in
    Response.Read.get_error t.conn.buffer t.conn.ic res >>= function
    | Some err ->
        Logs.err (fun l -> l "Request error: command=%s, error=%s" name err);
        Lwt.return_error (`Msg err)
    | None ->
        let+ x = Conn.read_message t.conn ty in
        Logs.debug (fun l -> l "Completed request: command=%s" name);
        x

  let request (t : t) (type x y)
      (module Cmd : C.CMD with type Res.t = x and type Req.t = y) (a : y) =
    let name = Cmd.name in
    Logs.debug (fun l -> l "Starting request: command=%s" name);
    handle_disconnect t (fun () ->
        let* () = send_command_header t (module Cmd) in
        let* () = Conn.write_message t.conn Cmd.Req.t a in
        let* () = Lwt_io.flush t.conn.oc in
        recv t name Cmd.Res.t)

  let recv_commit_diff (t : t) =
    Lwt.catch
      (fun () ->
        Conn.read_message t.conn (Irmin.Diff.t Commit.t) >>= function
        | Ok x -> Lwt.return_some x
        | Error e ->
            Logs.err (fun l -> l "Watch error: %s" (Error.to_string e));
            Lwt.return_none)
      (fun _ -> Lwt.return_none)

  module Cache = struct
    module Hash = Irmin.Private.Lru.Make (struct
      type t = Hash.t

      let hash = Hashtbl.hash

      let hash_equal = Irmin.Type.(unstage (equal Hash.t))

      let equal a b = hash_equal a b
    end)

    let hash_commit : commit Hash.t = Hash.create 64

    let contents : contents Hash.t = Hash.create 64
  end

  let stats t = request t (module Commands.Stats) ()

  let ping t = request t (module Commands.Ping) ()

  let export t = request t (module Commands.Export) ()

  let import t slice = request t (module Commands.Import) slice

  let unwatch t = request t (module Commands.Unwatch) ()

  let watch f t =
    request t (module Commands.Watch) () >>= function
    | Error e -> Lwt.return_error e
    | Ok () -> (
        let rec loop () =
          recv_commit_diff t >>= function
          | Some diff -> (
              f diff >>= function
              | Ok `Continue -> loop ()
              | Ok `Stop -> Lwt.return_ok ()
              | Error e -> Lwt.return_error e)
          | None ->
              let* () = Lwt_unix.sleep 0.25 in
              loop ()
        in
        loop () >>= function
        | Ok () -> unwatch t
        | Error e ->
            let* _ = unwatch t in
            Lwt.return_error e)

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

  module Store = struct
    let find t key = request t (module Commands.Store.Find) key

    let set t ~info key value =
      request t (module Commands.Store.Set) (key, info (), value)

    let test_and_set t ~info key ~test ~set =
      request t (module Commands.Store.Test_and_set) (key, info (), (test, set))

    let remove t ~info key =
      request t (module Commands.Store.Remove) (key, info ())

    let find_tree t key =
      let+ tree = request t (module Commands.Store.Find_tree) key in
      Result.map (fun x -> Option.map (fun x -> (t, x, [])) x) tree

    let set_tree t ~info key (_, tree, _) =
      let+ tree =
        request t (module Commands.Store.Set_tree) (key, info (), tree)
      in
      Result.map (fun tree -> (t, tree, [])) tree

    let test_and_set_tree t ~info key ~test ~set =
      let test = Option.map (fun (_, x, _) -> x) test in
      let set = Option.map (fun (_, x, _) -> x) set in
      let+ tree =
        request t
          (module Commands.Store.Test_and_set_tree)
          (key, info (), (test, set))
      in
      Result.map (Option.map (fun tree -> (t, tree, []))) tree

    let mem t key = request t (module Commands.Store.Mem) key

    let mem_tree t key = request t (module Commands.Store.Mem_tree) key

    let merge t ~info branch =
      request t (module Commands.Store.Merge) (info (), branch)

    let merge_commit t ~info commit =
      request t (module Commands.Store.Merge_commit) (info (), commit)

    let last_modified t key =
      request t (module Commands.Store.Last_modified) key
  end

  module Contents = struct
    include St.Contents

    let of_hash t hash =
      if Cache.Hash.mem Cache.contents hash then
        Lwt.return_ok (Some (Cache.Hash.find Cache.contents hash))
      else request t (module Commands.Contents_of_hash) hash

    let exists' t contents =
      let hash = hash contents in
      if Cache.Hash.mem Cache.contents hash then Lwt.return_ok (hash, true)
      else
        let* res = request t (module Commands.Contents_exists) hash in
        match res with
        | Ok true ->
            Cache.Hash.add Cache.contents hash contents;
            Lwt.return_ok (hash, true)
        | x -> Lwt.return (Result.map (fun y -> (hash, y)) x)

    let exists t contents = exists' t contents >|= Result.map snd

    let save t contents =
      let hash = hash contents in
      if Cache.Hash.mem Cache.contents hash then Lwt.return_ok hash
      else request t (module Commands.Contents_save) contents
  end

  module Tree = struct
    type store = t

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
      | b -> add_batch (t, tree, b) []

    and add_batch (((t : store), tree, batch) : tree) l =
      wrap t
        (request t
           (module Commands.Tree.Add_batch)
           (tree, List.rev_append batch (List.rev l)))

    and wrap ?(batch = []) store tree =
      let* tree = tree in
      Lwt.return (Result.map (fun tree -> (store, tree, batch)) tree)

    and empty (t : store) : tree = (t, Tree.Local (`Tree []), [])

    module Batch = struct
      let key_equal = Irmin.Type.(unstage (equal Key.t))

      let find b k =
        List.find_opt
          (fun (a, b) ->
            match b with `Contents _ -> key_equal k a | _ -> false)
          b
        |> Option.map snd

      let find_tree b k =
        List.find_opt
          (fun (a, b) -> match b with `Tree _ -> key_equal k a | _ -> false)
          b
        |> Option.map snd

      let mem b k =
        List.exists
          (fun (a, b) ->
            match b with `Contents _ -> key_equal k a | _ -> false)
          b

      let mem_tree b k =
        List.exists
          (fun (a, b) -> match b with `Tree _ -> key_equal k a | _ -> false)
          b

      let remove b k = List.filter (fun (a, _) -> not (key_equal k a)) b

      let add batch key value = (key, `Contents (`Value value)) :: batch

      let add_hash batch key hash = (key, `Contents (`Hash hash)) :: batch

      let add_tree batch key (_, tree, batch') =
        (key, `Tree tree) :: batch' @ batch
    end

    let split t = t

    let v t ?(batch = []) tr = (t, tr, batch)

    let of_hash t hash = (t, Private.Tree.Hash hash, [])

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

    let hash (t, tree, batch) =
      let* tree = build t ~tree batch in
      map_tree tree (fun tree -> request t (module Commands.Tree.Hash) tree)

    let add' (t, tree, batch) key value =
      wrap ~batch t (request t (module Commands.Tree.Add) (tree, key, value))

    let add ((t, tree, batch) : tree) key value =
      let hash = St.Contents.hash value in
      let exists = Cache.Hash.mem Cache.contents hash in
      let batch =
        if exists then Batch.add_hash batch key hash
        else Batch.add batch key value
      in
      if List.length batch > t.conf.batch_size then build t ~tree batch
      else Lwt.return_ok (t, tree, batch)

    let add_tree ((t, tree, batch) : tree) key tr =
      let batch = Batch.add_tree batch key tr in
      if List.length batch > t.conf.batch_size then build t ~tree batch
      else Lwt.return_ok (t, tree, batch)

    let add_tree' (t, tree, batch) key (_, tr, batch') =
      wrap ~batch:(batch @ batch') t
        (request t (module Commands.Tree.Add_tree) (tree, key, tr))

    let find ((t, tree, batch) : tree) key : contents option Error.result Lwt.t
        =
      let x = Batch.find batch key in
      match x with
      | Some (`Contents (`Value x)) -> Lwt.return_ok (Some x)
      | Some (`Contents (`Hash x)) -> Contents.of_hash t x
      | _ -> request t (module Commands.Tree.Find) (tree, key)

    let find_tree ((t, tree, batch) : tree) key : tree option Error.result Lwt.t
        =
      let x = Batch.find_tree batch key in
      match x with
      | Some (`Tree x) -> Lwt.return_ok (Some (t, x, []))
      | _ ->
          let+ tree = request t (module Commands.Tree.Find_tree) (tree, key) in
          Result.map (Option.map (fun tree -> (t, tree, []))) tree

    let remove (t, tree, batch) key =
      let batch = Batch.remove batch key in
      wrap ~batch t (request t (module Commands.Tree.Remove) (tree, key))

    let cleanup (t, tree, _) = request t (module Commands.Tree.Cleanup) tree

    let mem (t, tree, batch) key =
      if Batch.mem batch key then Lwt.return_ok true
      else request t (module Commands.Tree.Mem) (tree, key)

    let mem_tree (t, tree, batch) key =
      if Batch.mem_tree batch key then Lwt.return_ok true
      else request t (module Commands.Tree.Mem_tree) (tree, key)

    let list (t, tree, batch) key =
      let* tree = build t ~tree batch in
      map_tree tree (fun tree ->
          request t (module Commands.Tree.List) (tree, key))

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

    let cleanup_all t = request t (module Commands.Tree.Cleanup_all) ()

    type t = tree
  end

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
              let hash = Commit.hash x in
              Cache.Hash.add Cache.hash_commit hash x;
              Ok x)

    let of_hash t hash =
      if Cache.(Hash.mem hash_commit hash) then
        Lwt.return_ok (Some Cache.(Hash.find hash_commit hash))
      else
        let* commit = request t (module Commands.Commit_of_hash) hash in
        match commit with
        | Ok c ->
            Option.iter (Cache.Hash.add Cache.hash_commit hash) c;
            Lwt.return_ok c
        | Error e -> Lwt.return_error e

    let tree t commit = (t, tree commit, [])
  end
end
