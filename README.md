# irmin-server

A high-performance server for [irmin](https://github.com/mirage/irmin) using a custom wire [protocol](#protocol).

## Command-line

To run the server:

```shell
$ dune exec bin/main.exe -- --root ./data
```

For more information:

```shell
$ dune exec bin/main.exe --help
```

## OCaml client

See [src/client_intf.ml](https://github.com/zshipko/irmin-server/blob/master/src/client_intf.ml)

## Protocol

### Message

Message `data` is encoded using `Irmin.Type.to_bin_string`

| Field  | Type                 |
| ------ | -------------------- |
| length | int32, little-endian |
| data   | Bytes                |


### Request

A request is sent from the client to the server

| Field               | Type                        |
| ------------------- | --------------------------- |
| command             | Newline delimited string    |
| message count       | int32, little-endian        |
| messages            | Zero or more Messages       |

### Response

A `response` is sent from the server back to the client after a `request` has been handled

Responses with no messages (i.e. `message count` = 0) are used to signal an `OK` response with no data

An error response is marked by setting `message count` to `-1`. It should always be followed 
by a single `string` Message containing a description of the error.

| Field           | Type                   |
| --------------- | ---------------------- |
| message count   | int32, little-endian   |
| messages        | Zero or more Messages |

### Handshake

A handshake is performed when a client connects to the server

#### V1
| Field   | Type                     |
| ------- | ------------------------ |
| version | Newline delimited string |

