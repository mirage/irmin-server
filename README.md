# irmin-server

A high-performance server for [irmin](https://github.com/mirage/irmin)

## Protocol

### Item

| Field | Type |
| ----- | ---- |
| Length | int32, little-endian |
| Data   | Bytes |


### Request

| Field     | Type     |
| -------- | -------- |
| Command  | Newline delimited string    |
| Number of arguments | int32, little-endian |
| Arguments | Zero or more `Item` |

### Response

| Field | Type |
| ----- | ---- |
| Number of items | int32, little-endian |
| Items | Zero or more `Item` |

### Handshake

#### V1
| Field | Type |
| ----- | ---- |
| Version | Newline delimited string |

