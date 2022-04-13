# Protocol

## Message

Message `data` is encoded using either `Irmin.Type.to_bin_string` or `Irmin.Type.to_json_string`,
depending on which codec is selected at runtime.

| Field  | Type                 |
| ------ | -------------------- |
| length | int64, big endian    |
| data   | bytes                |


## Request

A request is sent from the client to the server

| Field               | Type                        |
| ------------------- | --------------------------- |
| command name length | uint8                       |
| command             | string                      |
| request             | Encoded message             |

## Response

A `response` is sent from the server back to the client after a `request` has been handled


An error response is marked by setting `status` >= `1`. It should always be followed
by a `string` Message containing a description of the error.

A successful response is marked by setting `status` to `0`.

| Field           | Type                   |
| --------------- | ---------------------- |
| status          | uint8                  |
| response        | Message                |

## Handshake

A handshake is performed when a client connects to the server

### V1

The following is sent as a request from the client to server **AND** the response from server to client

| Field        | Type                      |
| -------      | ------------------------- |
| version hash | `\n` delimited string     |

`contents type hash` is the protocol version appended to the hex-encoded hash of the name of the store contents type using `Store.Hash`. The name of the content type is determined by `Irmin.Type.pp_ty`.

For example, for a store with BLAKE2B hash and string contents the `contents type hash` is equal to `V1 + BLAKE2B("Custom (string)")`

This is used as a basic sanity check to ensure the client and server have the same hash and contents.
