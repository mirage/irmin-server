# irmin-server

A high-performance server for [irmin](https://github.com/mirage/irmin)

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
| messages            | Zero or more `Message`      |

### Response

A `response` is sent from the server back to the client after a `request` has been handled

Responses with no messages (i.e. `message count` = 0) are used to signal an `OK` response with no data

| Field           | Type                   |
| --------------- | ---------------------- |
| message count   | int32, little-endian   |
| messages        | Zero or more `Message` |

### Handshake

A handshake is performed when a client connects to the server

#### V1
| Field   | Type                     |
| ------- | ------------------------ |
| version | Newline delimited string |

