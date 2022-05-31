open Lwt.Syntax

module Make
    (IO : Conn.IO)
    (Codec : Conn.Codec.S)
    (Store : Irmin.Generic_key.S)
    (Tree : Tree.S
              with module Private.Store = Store
               and type Local.t = Store.tree)
    (Commit : Commit.S
                with type hash = Store.hash
                 and type tree = Tree.t
                 and type key = Store.commit_key
                 and module Info = Store.Info) =
struct
  include Context.Make (IO) (Codec) (Store) (Tree)
  module Return = Conn.Return
  module Schema = Store.Schema
  module Hash = Store.Hash
  open Store.Backend

  module Contents = struct
    type key = Contents.key

    let key_t = Contents.Key.t

    type value = Contents.value

    let value_t = Contents.Val.t

    type hash = Contents.hash

    let hash_t = Contents.Hash.t

    module Mem = struct
      let name = "x.contents.mem"

      type req = key [@@deriving irmin]
      type res = bool [@@deriving irmin]

      let run conn ctx _ key =
        let* x =
          Repo.batch ctx.repo (fun contents _ _ -> Contents.mem contents key)
        in
        Return.v conn res_t x
    end

    module Find = struct
      let name = "x.contents.find"

      type req = key [@@deriving irmin]
      type res = value option [@@deriving irmin]

      let run conn ctx _ key =
        let* v =
          Repo.batch ctx.repo (fun contents _ _ -> Contents.find contents key)
        in
        Return.v conn res_t v
    end

    module Add = struct
      let name = "x.contents.add"

      type req = value [@@deriving irmin]
      type res = key [@@deriving irmin]

      let run conn ctx _ value =
        let* k =
          Repo.batch ctx.repo (fun contents _ _ -> Contents.add contents value)
        in
        Return.v conn res_t k
    end

    module Unsafe_add = struct
      let name = "x.contents.unsafe_add"

      type req = hash * value [@@deriving irmin]
      type res = key [@@deriving irmin]

      let run conn ctx _ (hash, value) =
        let* k =
          Repo.batch ctx.repo (fun contents _ _ ->
              Contents.unsafe_add contents hash value)
        in
        Return.v conn res_t k
    end

    module Index = struct
      let name = "x.contents.index"

      type req = hash [@@deriving irmin]
      type res = key option [@@deriving irmin]

      let run conn ctx _ hash =
        let* v =
          Repo.batch ctx.repo (fun contents _ _ -> Contents.index contents hash)
        in
        Return.v conn res_t v
    end

    module Clear = struct
      let name = "x.contents.clear"

      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn ctx _ () =
        let* () =
          Repo.batch ctx.repo (fun contents _ _ -> Contents.clear contents)
        in
        Return.v conn res_t ()
    end

    module Merge = struct
      let name = "x.contents.merge"

      type req = key option option * key option * key option [@@deriving irmin]
      type res = (key option, Irmin.Merge.conflict) Result.t [@@deriving irmin]

      let run conn ctx _ (old, a, b) =
        let* res =
          Repo.batch ctx.repo (fun contents _ _ ->
              let merge = Contents.merge contents in
              let f = Irmin.Merge.f merge in
              let old () = Lwt.return_ok old in
              f ~old a b)
        in
        Return.v conn res_t res
    end
  end

  module Node = struct
    type key = Node.key

    let key_t = Node.Key.t

    type value = Node.value

    let value_t = Node.Val.t

    type hash = Hash.t

    module Mem = struct
      let name = "x.node.mem"

      type req = key [@@deriving irmin]
      type res = bool [@@deriving irmin]

      let run conn ctx _ key =
        let* x = Repo.batch ctx.repo (fun _ node _ -> Node.mem node key) in
        Return.v conn res_t x
    end

    module Find = struct
      let name = "x.node.find"

      type req = key [@@deriving irmin]
      type res = value option [@@deriving irmin]

      let run conn ctx _ key =
        let* v = Repo.batch ctx.repo (fun _ node _ -> Node.find node key) in
        Return.v conn res_t v
    end

    module Add = struct
      let name = "x.node.add"

      type req = value [@@deriving irmin]
      type res = key [@@deriving irmin]

      let run conn ctx _ value =
        let* k = Repo.batch ctx.repo (fun _ node _ -> Node.add node value) in
        Return.v conn res_t k
    end

    module Unsafe_add = struct
      let name = "x.node.unsafe_add"

      type req = Hash.t * value [@@deriving irmin]
      type res = key [@@deriving irmin]

      let run conn ctx _ (hash, value) =
        let* k =
          Repo.batch ctx.repo (fun _ node _ -> Node.unsafe_add node hash value)
        in
        Return.v conn res_t k
    end

    module Index = struct
      let name = "x.node.index"

      type req = Hash.t [@@deriving irmin]
      type res = key option [@@deriving irmin]

      let run conn ctx _ hash =
        let* v = Repo.batch ctx.repo (fun _ node _ -> Node.index node hash) in
        Return.v conn res_t v
    end

    module Clear = struct
      let name = "x.node.clear"

      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn ctx _ () =
        let* () = Repo.batch ctx.repo (fun _ node _ -> Node.clear node) in
        Return.v conn res_t ()
    end

    module Merge = struct
      let name = "x.node.merge"

      type req = key option option * key option * key option [@@deriving irmin]
      type res = (key option, Irmin.Merge.conflict) Result.t [@@deriving irmin]

      let run conn ctx _ (old, a, b) =
        let* res =
          Repo.batch ctx.repo (fun _ node _ ->
              let merge = Node.merge node in
              let f = Irmin.Merge.f merge in
              let old () = Lwt.return_ok old in
              f ~old a b)
        in
        Return.v conn res_t res
    end
  end

  module Commit = struct
    type key = Commit.key

    let key_t = Commit.Key.t

    type value = Commit.value

    let value_t = Commit.Val.t

    type hash = Hash.t

    module Mem = struct
      let name = "x.commit.mem"

      type req = key [@@deriving irmin]
      type res = bool [@@deriving irmin]

      let run conn ctx _ key =
        let x = Repo.commit_t ctx.repo in
        let* v = Commit.mem x key in
        Return.v conn res_t v
    end

    module Find = struct
      let name = "x.commit.find"

      type req = key [@@deriving irmin]
      type res = value option [@@deriving irmin]

      let run conn ctx _ key =
        let x = Repo.commit_t ctx.repo in
        let* v = Commit.find x key in
        Return.v conn res_t v
    end

    module Add = struct
      let name = "x.commit.add"

      type req = value [@@deriving irmin]
      type res = key [@@deriving irmin]

      let run conn ctx _ value =
        let* k =
          Repo.batch ctx.repo (fun _ _ commit -> Commit.add commit value)
        in
        Return.v conn res_t k
    end

    module Unsafe_add = struct
      let name = "x.commit.unsafe_add"

      type req = Hash.t * value [@@deriving irmin]
      type res = key [@@deriving irmin]

      let run conn ctx _ (hash, value) =
        let* k =
          Repo.batch ctx.repo (fun _ _ commit ->
              Commit.unsafe_add commit hash value)
        in
        Return.v conn res_t k
    end

    module Index = struct
      let name = "x.commit.index"

      type req = Hash.t [@@deriving irmin]
      type res = key option [@@deriving irmin]

      let run conn ctx _ hash =
        let* v =
          Repo.batch ctx.repo (fun _ _ commit -> Commit.index commit hash)
        in
        Return.v conn res_t v
    end

    module Clear = struct
      let name = "x.commit.clear"

      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn ctx _ () =
        let* () = Repo.batch ctx.repo (fun _ _ commit -> Commit.clear commit) in
        Return.v conn res_t ()
    end

    module Merge = struct
      let name = "x.commit.merge"

      type req = Store.Info.t * (key option option * key option * key option)
      [@@deriving irmin]

      type res = (key option, Irmin.Merge.conflict) Result.t [@@deriving irmin]

      let run conn ctx _ (info, (old, a, b)) =
        let info () = info in
        let* res =
          Repo.batch ctx.repo (fun _ _ commit ->
              let merge = Commit.merge commit ~info in
              let f = Irmin.Merge.f merge in
              let old () = Lwt.return_ok old in
              f ~old a b)
        in
        Return.v conn res_t res
    end
  end

  module Branch = struct
    type key = Schema.Branch.t [@@deriving irmin]
    type value = Store.commit_key [@@deriving irmin]

    module Mem = struct
      let name = "x.branch.mem"

      type req = key [@@deriving irmin]
      type res = bool [@@deriving irmin]

      let run conn ctx _ branch =
        let b = Repo.branch_t ctx.repo in
        let* x = Branch.mem b branch in
        Return.v conn res_t x
    end

    module Find = struct
      let name = "x.branch.find"

      type req = key [@@deriving irmin]
      type res = value option [@@deriving irmin]

      let run conn ctx _ branch =
        let b = Repo.branch_t ctx.repo in
        let* commit = Branch.find b branch in
        Return.v conn res_t commit
    end

    module Set = struct
      let name = "x.branch.set"

      type req = key * value [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn ctx _ (branch, commit) =
        let b = Repo.branch_t ctx.repo in
        let* () = Branch.set b branch commit in
        Return.v conn res_t ()
    end

    module Test_and_set = struct
      let name = "x.branch.test_and_set"

      type req = key * value option * value option [@@deriving irmin]
      type res = bool [@@deriving irmin]

      let run conn ctx _ (branch, test, set) =
        let b = Repo.branch_t ctx.repo in
        let* res = Branch.test_and_set b branch ~test ~set in
        Return.v conn res_t res
    end

    module Remove = struct
      let name = "x.branch.remove"

      type req = key [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn ctx _ branch =
        let b = Repo.branch_t ctx.repo in
        let* () = Branch.remove b branch in
        Return.v conn res_t ()
    end

    module List = struct
      let name = "x.branch.list"

      type req = unit [@@deriving irmin]
      type res = key list [@@deriving irmin]

      let run conn ctx _ () =
        let b = Repo.branch_t ctx.repo in
        let* b = Branch.list b in
        Return.v conn res_t b
    end

    module Clear = struct
      let name = "x.branch.clear"

      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let run conn ctx _ () =
        let b = Repo.branch_t ctx.repo in
        let* () = Branch.clear b in
        Return.v conn res_t ()
    end

    module Watch = struct
      type req = (key * value) list option [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "x.branch.watch"

      let run conn ctx _ init =
        let b = Repo.branch_t ctx.repo in
        let* () =
          match ctx.branch_watch with
          | Some watch ->
              ctx.branch_watch <- None;
              Branch.unwatch b watch
          | None -> Lwt.return_unit
        in
        let* watch =
          Branch.watch b ?init (fun key diff ->
              let diff_t = Irmin.Diff.t Store.commit_key_t in
              Lwt.catch
                (fun () ->
                  let* () = Conn.Response.write_header conn { status = 0 } in
                  let* () =
                    Conn.write conn
                      (Irmin.Type.pair Store.Branch.t diff_t)
                      (key, diff)
                  in
                  IO.flush conn.oc)
                (fun _ -> Lwt.return_unit))
        in
        ctx.branch_watch <- Some watch;
        Return.v conn res_t ()
    end

    module Watch_key = struct
      type req = value option * key [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "x.branch.watch_key"

      let run conn ctx _ (init, key) =
        let b = Repo.branch_t ctx.repo in
        let* () =
          match ctx.branch_watch with
          | Some watch ->
              ctx.branch_watch <- None;
              Branch.unwatch b watch
          | None -> Lwt.return_unit
        in
        let* watch =
          Branch.watch_key b key ?init (fun diff ->
              let diff_t = Irmin.Diff.t Store.commit_key_t in
              Lwt.catch
                (fun () ->
                  let* () = Conn.Response.write_header conn { status = 0 } in
                  let* () = Conn.write conn diff_t diff in
                  IO.flush conn.oc)
                (fun _ -> Lwt.return_unit))
        in
        ctx.branch_watch <- Some watch;
        Return.v conn res_t ()
    end

    module Unwatch = struct
      type req = unit [@@deriving irmin]
      type res = unit [@@deriving irmin]

      let name = "x.branch.unwatch"

      let run conn ctx _ () =
        let b = Repo.branch_t ctx.repo in
        let* () =
          match ctx.branch_watch with
          | Some watch ->
              ctx.branch_watch <- None;
              Branch.unwatch b watch
          | None -> Lwt.return_unit
        in
        Return.v conn res_t ()
    end
  end

  let commands =
    [
      cmd (module Contents.Mem);
      cmd (module Contents.Find);
      cmd (module Contents.Add);
      cmd (module Contents.Unsafe_add);
      cmd (module Contents.Index);
      cmd (module Contents.Clear);
      cmd (module Contents.Merge);
      cmd (module Node.Mem);
      cmd (module Node.Find);
      cmd (module Node.Add);
      cmd (module Node.Unsafe_add);
      cmd (module Node.Index);
      cmd (module Node.Clear);
      cmd (module Node.Merge);
      cmd (module Commit.Mem);
      cmd (module Commit.Find);
      cmd (module Commit.Add);
      cmd (module Commit.Unsafe_add);
      cmd (module Commit.Index);
      cmd (module Commit.Clear);
      cmd (module Commit.Merge);
      cmd (module Branch.Mem);
      cmd (module Branch.Find);
      cmd (module Branch.Set);
      cmd (module Branch.Test_and_set);
      cmd (module Branch.Remove);
      cmd (module Branch.List);
      cmd (module Branch.Clear);
      cmd (module Branch.Watch);
      cmd (module Branch.Unwatch);
      cmd (module Branch.Watch_key);
    ]
end
