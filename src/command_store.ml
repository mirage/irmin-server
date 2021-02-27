open Lwt.Syntax
open Lwt.Infix

module Make (Store : Irmin_pack_layered.S) = struct
  include Context.Make (Store)

  module Find = struct
    type req = Store.key

    type res = Store.contents option

    let args = (1, 1)

    let name = "store.find"

    module Server = struct
      let handle conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* x = Store.find ctx.store key in
        let* () = Conn.begin_response ctx.conn 1 in
        Return.v conn (Irmin.Type.option Store.contents_t) x
    end

    module Client = struct
      let send t key = Args.write t Store.Key.t key

      let recv args = Args.next args (Irmin.Type.option Store.contents_t)
    end
  end

  module Set = struct
    type req = Store.key * Irmin.Info.t * Store.contents

    type res = unit

    let args = (3, 0)

    let name = "store.set"

    module Server = struct
      let handle conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* info = Args.next args Irmin.Info.t >|= Error.unwrap in
        let* contents = Args.next args Store.Contents.t >|= Error.unwrap in
        let* () = Store.set_exn ctx.store key ~info:(fun () -> info) contents in
        Return.ok conn
    end

    module Client = struct
      let send t (key, info, contents) =
        let* () = Args.write t Store.Key.t key in
        let* () = Args.write t Irmin.Info.t info in
        Args.write t Store.contents_t contents

      let recv _args = Lwt.return_ok ()
    end
  end

  module Remove = struct
    type req = Store.key * Irmin.Info.t

    type res = unit

    let args = (2, 0)

    let name = "store.remove"

    module Server = struct
      let handle conn ctx args =
        let* key = Args.next args Store.Key.t >|= Error.unwrap in
        let* info = Args.next args Irmin.Info.t >|= Error.unwrap in
        let* () = Store.remove_exn ctx.store key ~info:(fun () -> info) in
        Return.ok conn
    end

    module Client = struct
      let send t (key, info) =
        let* () = Args.write t Store.Key.t key in
        Args.write t Irmin.Info.t info

      let recv _args = Lwt.return_ok ()
    end
  end
end
