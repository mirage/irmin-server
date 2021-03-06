open Lwt.Syntax
open Lwt.Infix
include Client_intf

module Make (C : Command.S with type Store.key = string list) = struct
  module St = C.Store
  open C
  module Hash = Store.Hash
  module Contents = Store.Contents
  module Key = Store.Key
  module Branch = Store.Branch
  module Commit_impl = Irmin.Private.Commit.Make (Hash)

  module Private = struct
    module Tree = C.Tree
  end

  type t = { client : Conduit_lwt_unix.client; mutable conn : Conn.t }

  type hash = Store.hash

  type contents = Store.contents

  type branch = Store.branch

  type key = Store.key

  type commit = Commit_impl.t

  type tree = t * Private.Tree.t

  type conf = Conduit_lwt_unix.client

  let conf ?(tls = false) ~uri () =
    let uri = Uri.of_string uri in
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
    let ip = Unix.gethostbyname addr in
    let client =
      match String.lowercase_ascii scheme with
      | "unix" -> `Unix_domain_socket (`File (Uri.path uri))
      | "tcp" ->
          let port = Uri.port uri |> Option.value ~default:8888 in
          let x = Random.int (Array.length ip.h_addr_list) in
          let ip =
            ip.h_addr_list.(x) |> Unix.string_of_inet_addr
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
    Lwt.catch
      (fun () ->
        let* x = f () in
        let+ () = Lwt_io.flush t.conn.oc in
        x)
      (function
        | End_of_file ->
            Logs.info (fun l -> l "Reconnecting to server");
            let* conn = connect' t.client in
            t.conn <- conn.conn;
            f ()
        | exn -> raise exn)

  let send_command_header t (module Cmd : C.CMD) =
    let n_args = fst Cmd.args in
    let header = Request.Header.v ~command:Cmd.name ~n_args in
    Request.Write.header t.conn.Conn.oc header

  let request t (type x y)
      (module Cmd : C.CMD with type res = x and type req = y) (a : y) =
    let name = Cmd.name in
    Logs.debug (fun l -> l "Starting request: command=%s" name);
    handle_disconnect t (fun () ->
        let* () = send_command_header t (module Cmd) in
        let args = Args.v ~mode:`Write ~count:(fst Cmd.args) t.conn in
        let* () = Cmd.Client.send args a in
        let* () = Lwt_io.flush t.conn.oc in
        let* res = Response.Read.header t.conn.ic in
        Response.Read.get_error t.conn.ic res >>= function
        | Some err ->
            Logs.err (fun l -> l "Request error: command=%s, error=%s" name err);
            Lwt.return_error (`Msg err)
        | None ->
            let args = Args.v ~mode:`Read ~count:res.n_items t.conn in
            let+ x = Cmd.Client.recv args in
            assert (Args.remaining args = 0);
            Logs.debug (fun l -> l "Completed request: command=%s" name);
            x)

  let ping t = request t (module Commands.Ping) ()

  let set_branch t (branch : Store.branch) =
    request t (module Commands.Set_branch) branch

  let get_branch t =
    request t (module Commands.Get_branch) ()

  module Store = struct
    let find t key = request t (module Commands.Store.Find) key

    let set t ~info key value =
      request t (module Commands.Store.Set) (key, info (), value)

    let test_and_set t ~info key ~test ~set =
      request t (module Commands.Store.Test_and_set) (key, info (), test, set)

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
          (key, info (), test, set)
      in
      Result.map (Option.map (fun tree -> (t, tree))) tree

    let mem t key = request t (module Commands.Store.Mem) key

    let mem_tree t key = request t (module Commands.Store.Mem_tree) key
  end

  module Tree = struct
    type store = t

    type t = store * Private.Tree.t

    let wrap store tree =
      let* tree = tree in
      Lwt.return (Result.map (fun tree -> (store, tree)) tree)

    let empty t = wrap t (request t (module Commands.Tree.Empty) ())

    let add (t, tree) key value =
      wrap t (request t (module Commands.Tree.Add) (tree, key, value))

    let remove (t, tree) key =
      wrap t (request t (module Commands.Tree.Remove) (tree, key))

    let abort (t, tree) = request t (module Commands.Tree.Abort) tree

    let mem (t, tree) key = request t (module Commands.Tree.Mem) (tree, key)

    let mem_tree (t, tree) key =
      request t (module Commands.Tree.Mem_tree) (tree, key)

    let list (t, tree) key = request t (module Commands.Tree.List) (tree, key)

    module Local = Private.Tree.Local

    let of_local t x = (t, Private.Tree.Local x)
  end

  module Commit = struct
    include Commit_impl
  end
end
