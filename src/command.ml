open Lwt.Syntax
open Lwt.Infix
include Command_intf

let of_name name =
  Irmin.Type.of_string command_t ("\"" ^ name ^ "\"") |> Error.unwrap

let name x =
  let s = Irmin.Type.to_string command_t x in
  if String.length s < 2 then invalid_arg "invalid command name"
  else String.sub s 1 (String.length s - 2)

module Make (St : Irmin.S) = struct
  type t = command

  module Tree = Tree.Make (St)
  module Store = St

  type context = {
    conn : Conn.t;
    repo : Store.Repo.t;
    mutable store : Store.t;
    trees : (int, Store.tree) Hashtbl.t;
  }

  type f = Conn.t -> context -> Args.t -> Return.t Lwt.t

  module X = struct
    let ping conn _ctx _args = Return.ok conn

    let set_branch conn ctx args =
      let* branch = Args.next args Store.Branch.t >|= Error.unwrap in
      let* store = Store.of_branch ctx.repo branch in
      ctx.store <- store;
      Return.ok conn

    module XTree = struct
      let resolve ctx tree =
        let* id, tree =
          match tree with
          | Tree.ID x -> Lwt.return @@ (Some x, Hashtbl.find_opt ctx.trees x)
          | Hash x ->
              Store.Tree.of_hash ctx.repo (`Node x) >|= fun x -> (None, x)
          | Local x -> Lwt.return (None, Some x)
        in
        match tree with
        | Some t -> Lwt.return (id, t)
        | None -> Error.raise_error 0 "ERROR unknown tree"

      let return_tree conn ctx id tree =
        let hash = Store.Tree.hash tree in
        Store.Tree.of_hash ctx.repo (`Node hash) >>= fun x ->
        match (x, id) with
        | Some _, id ->
            Option.iter (Hashtbl.remove ctx.trees) id;
            Return.v conn Tree.t (Hash hash)
        | None, Some id ->
            Hashtbl.replace ctx.trees id tree;
            Return.v conn Tree.t (ID id)
        | None, None ->
            let id = Random.bits () in
            Hashtbl.replace ctx.trees id tree;
            Return.v conn Tree.t (ID id)

      let empty conn ctx _args =
        let empty = St.Tree.empty in
        let id = Random.bits () in
        Hashtbl.replace ctx.trees id empty;
        Return.v conn Tree.t (ID id)

      let add conn ctx args =
        let* tree = Args.next args Tree.t >|= Error.unwrap in
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* value = Args.next args Store.contents_t >|= Error.unwrap in
        let* id, tree = resolve ctx tree in
        let* tree = Store.Tree.add tree key value in
        return_tree conn ctx id tree

      let remove conn ctx args =
        let* tree = Args.next args Tree.t >|= Error.unwrap in
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* id, tree = resolve ctx tree in
        let* tree = Store.Tree.remove tree key in
        return_tree conn ctx id tree
    end

    module XStore = struct
      let find conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* x = Store.find ctx.store key in
        let* () = Conn.begin_response ctx.conn 1 in
        Return.v conn (Irmin.Type.option Store.contents_t) x

      let set conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* info = Args.next args Irmin.Info.t >|= Error.unwrap in
        let* value = Args.next args Store.Contents.t >|= Error.unwrap in
        let* () = Store.set_exn ctx.store key value ~info:(fun () -> info) in
        Return.ok conn

      let remove conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* info = Args.next args Irmin.Info.t >|= Error.unwrap in
        let* () = Store.remove_exn ctx.store key ~info:(fun () -> info) in
        Return.ok conn

      let find_tree conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* tree = Store.find_tree ctx.store key in
        let hash = Option.map (fun x -> Tree.Hash (Store.Tree.hash x)) tree in
        Return.v conn (Irmin.Type.option Tree.t) hash

      let set_tree conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* info = Args.next args Irmin.Info.t >|= Error.unwrap in
        let* tree = Args.next args Tree.t >|= Error.unwrap in
        let* id, tree = XTree.resolve ctx tree in
        let* () =
          Store.set_tree_exn ctx.store key tree ~info:(fun () -> info)
        in
        Option.iter (Hashtbl.remove ctx.trees) id;
        Return.ok conn
    end

    module Store = XStore
    module Tree = XTree
  end

  let cmd x n_in n_out f = (x, (n_in, n_out, f))

  let commands =
    let open X in
    [
      cmd Ping 0 0 ping;
      cmd SetBranch 1 0 set_branch;
      (* Store *)
      cmd Find 1 1 Store.find;
      cmd Set 3 0 Store.set;
      cmd Remove 2 0 Store.remove;
      cmd FindTree 1 1 Store.find_tree;
      cmd SetTree 3 0 Store.set_tree;
      (* Tree *)
      cmd TreeAdd 3 1 Tree.add;
      cmd TreeRemove 2 1 Tree.remove;
      cmd EmptyTree 0 1 Tree.empty;
    ]

  let n_args cmd =
    let n, _, _ = List.assoc cmd commands in
    n

  let n_results cmd =
    let _, n, _ = List.assoc cmd commands in
    n
end
