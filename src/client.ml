open Lwt.Syntax
open Lwt.Infix
include Client_intf

module Make (C : Command.S with type Store.key = string list) = struct
  module St = C.Store
  open C
  module Hash = Store.Hash
  module Key = Store.Key

  module Private = struct
    module Tree = C.Tree
  end

  type t = { client : Conduit_lwt_unix.client; mutable conn : Conn.t }

  type hash = Store.hash

  type contents = Store.contents

  type branch = Store.branch

  type key = Store.key

  type commit = C.Commit.t

  type tree = t * Private.Tree.t

  type slice = St.slice

  let slice_t = St.slice_t

  type conf = Conduit_lwt_unix.client

  let conf ?(tls = false) ~uri () =
    let uri = Uri.of_string uri in
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
    let client =
      match String.lowercase_ascii scheme with
      | "unix" -> `Unix_domain_socket (`File (Uri.path uri))
      | "tcp" ->
          let ip = Unix.gethostbyname addr in
          let port = Uri.port uri |> Option.value ~default:8888 in
          let ip =
            ip.h_addr_list.(0) |> Unix.string_of_inet_addr
            |> Ipaddr.of_string_exn
          in
          if not tls then `TCP (`IP ip, `Port port)
          else `TLS (`Hostname addr, `IP ip, `Port port)
      | x -> invalid_arg ("Unknown client scheme: " ^ x)
    in
    client

  let connect' client =
    let ctx = Conduit_lwt_unix.default_ctx in
    let* flow, ic, oc = Conduit_lwt_unix.connect ~ctx client in
    let conn = Conn.v flow ic oc in
    let+ () = Handshake.V1.send ic oc in
    { client; conn }

  let connect ?tls ~uri () =
    let client = conf ?tls ~uri () in
    connect' client

  let handle_disconnect t f =
    Lwt.catch f (function
      | End_of_file ->
          Logs.info (fun l -> l "Reconnecting to server");
          let* conn = connect' t.client in
          t.conn <- conn.conn;
          f ()
      | exn -> raise exn)
    [@@inline]

  let send_command_header t (module Cmd : C.CMD) =
    let header = Request.Header.v ~command:Cmd.name in
    Request.Write.header t.conn.Conn.oc header

  let request t (type x y)
      (module Cmd : C.CMD with type Res.t = x and type Req.t = y) (a : y) =
    let name = Cmd.name in
    Logs.debug (fun l -> l "Starting request: command=%s" name);
    handle_disconnect t (fun () ->
        let* () = send_command_header t (module Cmd) in
        let* () = Conn.write_message t.conn Cmd.Req.t a in
        let* () = Lwt_io.flush t.conn.oc in
        let* res = Response.Read.header t.conn.ic in
        Response.Read.get_error t.conn.buffer t.conn.ic res >>= function
        | Some err ->
            Logs.err (fun l -> l "Request error: command=%s, error=%s" name err);
            Lwt.return_error (`Msg err)
        | None ->
            let+ x = Conn.read_message t.conn Cmd.Res.t in
            Logs.debug (fun l -> l "Completed request: command=%s" name);
            x)

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

  let ping t = request t (module Commands.Ping) ()

  let flush t = request t (module Commands.Flush) ()

  let export t = request t (module Commands.Export) ()

  let import t slice = request t (module Commands.Import) slice

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
      Result.map (fun x -> Option.map (fun x -> (t, x)) x) tree

    let set_tree t ~info key (_, tree) =
      let+ tree =
        request t (module Commands.Store.Set_tree) (key, info (), tree)
      in
      Result.map (fun tree -> (t, tree)) tree

    let test_and_set_tree t ~info key ~test ~set =
      let test = Option.map snd test in
      let set = Option.map snd set in
      let+ tree =
        request t
          (module Commands.Store.Test_and_set_tree)
          (key, info (), (test, set))
      in
      Result.map (Option.map (fun tree -> (t, tree))) tree

    let mem t key = request t (module Commands.Store.Mem) key

    let mem_tree t key = request t (module Commands.Store.Mem_tree) key
  end

  module Contents = struct
    include St.Contents

    let of_hash t hash =
      if Cache.Hash.mem Cache.contents hash then
        Lwt.return_ok (Some (Cache.Hash.find Cache.contents hash))
      else request t (module Commands.Contents_of_hash) hash

    let mem t contents =
      let hash = hash contents in
      if Cache.Hash.mem Cache.contents hash then Lwt.return_ok true
      else
        let* res = request t (module Commands.Contents_mem) hash in
        match res with
        | Ok true ->
            Cache.Hash.add Cache.contents hash contents;
            Lwt.return_ok true
        | x -> Lwt.return x

    let save t contents =
      let hash = hash contents in
      if Cache.Hash.mem Cache.contents hash then Lwt.return_ok hash
      else request t (module Commands.Contents_save) contents
  end

  module Tree = struct
    type store = t

    type t = store * Private.Tree.t

    let wrap store tree =
      let* tree = tree in
      Lwt.return (Result.map (fun tree -> (store, tree)) tree)

    let empty t = wrap t (request t (module Commands.Tree.Empty) ())

    let split t = t

    let of_hash t hash = (t, Private.Tree.Hash hash)

    let clear (t, tree) = request t (module Commands.Tree.Clear) tree

    let hash (t, tree) = request t (module Commands.Tree.Hash) tree

    let list_ignore (t, tree) =
      request t (module Commands.Tree.List_ignore) tree

    let add' (t, tree) key value =
      wrap t (request t (module Commands.Tree.Add) (tree, key, value))

    let add_hash (t, tree) key hash =
      wrap t (request t (module Commands.Tree.Add_hash) (tree, key, hash))

    let add_multiple_hash (t, tree) l =
      wrap t (request t (module Commands.Tree.Add_multiple_hash) (tree, l))

    let add (t, tree) key value =
      let* exists = Contents.mem t value >|= Error.unwrap "Contents.mem" in
      if exists then
        let hash = Contents.hash value in
        add_hash (t, tree) key hash
      else add' (t, tree) key value

    let add_tree (t, tree) key (_, tr) =
      wrap t (request t (module Commands.Tree.Add_tree) (tree, key, tr))

    let find (t, tree) key = request t (module Commands.Tree.Find) (tree, key)

    let find_tree (t, tree) key =
      let+ tree = request t (module Commands.Tree.Find_tree) (tree, key) in
      Result.map (Option.map (fun tree -> (t, tree))) tree

    let remove (t, tree) key =
      wrap t (request t (module Commands.Tree.Remove) (tree, key))

    let cleanup (t, tree) = request t (module Commands.Tree.Cleanup) tree

    let mem (t, tree) key = request t (module Commands.Tree.Mem) (tree, key)

    let mem_tree (t, tree) key =
      request t (module Commands.Tree.Mem_tree) (tree, key)

    let list (t, tree) key = request t (module Commands.Tree.List) (tree, key)

    module Local = Private.Tree.Local

    let to_local (t, tree) =
      let+ res = request t (module Commands.Tree.To_local) tree in
      match res with
      | Ok x ->
          let x = Private.Tree.Local.of_concrete x in
          Ok x
      | Error e -> Error e

    let of_local t x =
      let+ x = Private.Tree.Local.to_concrete x in
      (t, Private.Tree.Local x)

    let reset_all t = request t (module Commands.Tree.Reset_all) ()

    type builder = store * (key * hash) list

    module Builder = struct
      let v t = (t, [])

      let add (t, b) k v =
        let* exists = Contents.mem t v >|= Error.unwrap "Contents.mem" in
        if exists then
          let hash = Contents.hash v in
          Lwt.return (t, (k, hash) :: b)
        else
          let* hash = Contents.save t v >|= Error.unwrap "Contents.save" in
          Lwt.return (t, (k, hash) :: b)

      let remove (t, b) k =
        let b = List.remove_assoc k b in
        (t, b)

      let build ?tree (t, b) : tree Error.result Lwt.t =
        let* tree =
          match tree with
          | Some tree -> Lwt.return tree
          | None -> empty t >|= Error.unwrap "empty"
        in
        add_multiple_hash tree b

      type t = builder
    end
  end

  module Commit = struct
    include C.Commit

    let v t ~info ~parents (_, tree) =
      request t (module Commands.Commit_v) (info (), parents, tree)

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

    let tree t commit = (t, tree commit)
  end
end
