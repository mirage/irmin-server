include Irmin.Backend.Conf

let spec = Irmin.Backend.Conf.Spec.v "irmin-client"
let uri = Irmin.Type.(map string) Uri.of_string Uri.to_string

module Key = struct
  let uri =
    Irmin.Backend.Conf.key ~spec "uri" uri
      (Uri.of_string "tcp://127.0.0.1:9181")

  let tls = Irmin.Backend.Conf.key ~spec "tls" Irmin.Type.bool false

  let hostname =
    Irmin.Backend.Conf.key ~spec "hostname" Irmin.Type.string "127.0.0.1"
end

let v ?(tls = false) ?hostname uri =
  let default_host = Uri.host_with_default ~default:"127.0.0.1" uri in
  let config =
    Irmin.Backend.Conf.add (Irmin.Backend.Conf.empty spec) Key.uri uri
  in
  let config =
    Irmin.Backend.Conf.add config Key.hostname
      (Option.value ~default:default_host hostname)
  in
  Irmin.Backend.Conf.add config Key.tls tls
