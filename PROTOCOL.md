# Protocol

## Message

Message `data` is encoded using `Irmin.Type.to_bin_string`

| Field  | Type                 |
| ------ | -------------------- |
| length | int64, big endian    |
| data   | bytes                |


## Request

A request is sent from the client to the server

| Field               | Type                        |
| ------------------- | --------------------------- |
| command             | `\n` delimited string       |
| request             | Message                     |

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

| Field              | Type                     |
| -------            | ------------------------ |
| contents type hash | `\n` delimited string    |

`contents hash type` is the hex-encoded hash of the **name** of the content type using `Store.Hash`.

For example, for a store with `string` contents and BLAKE2B hash the `contents type hash` is equal to `BLAKE2B("string")`

This is used as a basic sanity check to ensure the client and server have the same hash and contents implementation
