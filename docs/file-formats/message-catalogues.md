# Format of Message catalogue files.

Message catalogue files are POSIX's way of storing internationalised strings.  Each catalogue is
split into multiple sets of messages - each of which contains messages indexed by an unsigned ID.

The rest of this document describes the format.

## Global statements

All values are stored in little-endian format.

All offsets stored in a catalogue are relative to the start of the catalogue.

The assumption is that whilst a message catalogue may be >4GB (and so need 64-bit offsets) the
number of message sets will not be >= 2^32 and the number of messages in each set will also not
be >= 2^32.  Messages will also not be longer than 2^32 bytes (excluding terminating NUL).  So IDs
and lengths are stored as `uint32_t`, and offsets as `uint64_t`.

Elements in the file should be naturally aligned to the type size - in particular 64-bit values are
aligned to 8-byte boundaries.

## Header

A message catalogue starts with a header in the following format.

| Offset from start of header | Length (in bytes) | Type | Name | Value |
| :========================== | :================ | :=== | :=== | :==== |
| +0 | 4 | `char[4]` | msg_hdr_magic | `{ 'M', 'S', 'G', '\0' }` - Magic header |
| +4 | 1 | `uint8_t` | msg_hdr_version | `1` - File version |
| +5 | 7 | `unit8_t[7]` | - | RESERVED should be 0. |
| +12 | 4 | `uint32_t` | msg_hdr_set_count | Number of sets in this catalogue. |
| +16 | 8 | `uint64_t` | msg_file_length | Length of the file.
| +24 | `sizeof(set_hdr) * msg_hdr_set_count` | `set_hdr[msg_hdr_set_count]` | msg_set_hdrs | Description of sets |

The number of sets in a catalogue is **not** equal to the ID of the highest numbered set.  There may
be gaps in the set orders.  Entries in `set_hdr` must be in ascending set ID order.

## Set headers

Each `set_hdr` stores the following information:

| Offset from start of header | Length (in bytes) | Type | Name | Value |
| :========================== | :================ | :=== | :=== | :==== |
| +0 | 4 | `uint32_t` | set_id | Set ID |
| +4 | 4 | `uint32_t` | set_msg_count | Number of messages in the set |
| +8 | 8 | `uint64_t` | set_msg_array_offset | Offset to start of messages array |

Message arrays should be aligned to 8-byte boundaries.

## Message Arrays

Each set contains a message array mapping message IDs to strings.

Message arrays should be aligned to 8-byte boundaries.

Each element of the array is in the following format:

| Offset for start of elemnt | Length (in bytes) | Type | Name | Value |
| +0 | 4 | `uint32_t` | msg_id | Message ID |
| +4 | 4 | `uint32_t` | msg_length | Legth of message (including terminating NUL). |
| +8 | 8 | `uint64_t` | msg_offset | Offset to start of message text |

Following all message arrays are the strings.  Strings are stored with a terminating NUL
character.

It can be assumed that these will be treated as read only and so string pointers can be overlapped to
save space.
