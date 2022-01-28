# irmin-server

A server for [irmin](https://github.com/mirage/irmin) using a custom wire [protocol](#protocol), designed to have minimal overhead.

## Command-line

### Server

To run the server:

```shell
$ dune exec bin/server/server.exe -- --root ./data
```

For more information:

```shell
$ dune exec bin/server/server.exe -- --help
```

`irmin-server` can also be executed using `Docker`:

```shell
$ docker run --env PORT=9999 $(docker build -q .)
```

### Client

`irmin-client` is a command-line application that can be used to send commands to `irmin-server`

For a list of available commands:

```shell
$ dune exec bin/client/client/exe -- --help
```

For help with a specific command (for example, ping):

```shell
$ dine exec bin/client/client.exe -- ping --help
```

## OCaml client

An OCaml client library is provided to interact with the server. Some examples can be
found in the [examples/](/tree/master/examples) directory.

### Docs

See [src/irmin-client/client_intf.ml](/blob/master/src/irmin-client/client_intf.ml)

## Protocol

A specification of the wire protocol can be found in [PROTOCOL.md](/blob/master/PROTOCOL.md)
