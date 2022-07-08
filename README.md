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

`irmin-server` accept `WebSocket` connection.
To run the server:

```shell
$ dune exec -- ./examples/server.exe --uri=ws://localhost:9090/ws
```

`irmin-server` can also be executed using `Docker`:

```shell
$ docker run --env PORT=9999 $(docker build -q .)
```

### Client

`irmin-client` is a command-line application that can be used to send commands to `irmin-server`

For a list of available commands:

```shell
$ dune exec bin/client/client.exe -- --help
```

For help with a specific command (for example, ping):

```shell
$ dune exec bin/client/client.exe -- ping --help
```

`irmin-client` can also send commands to `irmin-server` via WebSocket.
To run an example thats ping the server:

```shell
$ dune exec ./examples/ping.exe ws://localhost:9090/ws
```

## Browser

### Server

`irmin-server` accept `WebSocket` connection which makes communication from a browser possible.
To run the server:

```shell
$ dune exec -- ./examples/server.exe --uri=ws://localhost:9090/ws
```

### Client

`irmin-client-jsoo` is a browser application that can be used to send commands to `irmin-server` via WebSocket    
`cd` into examples/js

```shell
$ dune build
```

To run an example thats ping the server from the browser:
Open the file `_build/default/examples/js/index.html` in the browser

## OCaml client

An OCaml client library is provided to interact with the server. Some examples can be
found in the [examples/](/examples) directory.

### Docs

See [src/irmin-client/client_intf.ml](/src/irmin-client/client_intf.ml)

## Protocol

A specification of the wire protocol can be found in [PROTOCOL.md](/PROTOCOL.md)
