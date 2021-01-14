# Implementation Limits

The following table lists the implementation limits we have chosen for the support library to expose
where we don't use the system defaults.

| Limit | Value | Restrictions | Description |
| :---- | :---- | :----------- | :---------- |
| `_POSIX2_LINE_MAX` | 2048 | Specified by standard | Maximum acceptable length, in bytes, of a line of input from a text file. |
| `_POSIX2_SSIZE_MAX` | 32767 | Specified by standard | The minimum acceptable maximum value for an object of type `ssize_t`. |
| `NL_MSGMAX` | `INT_MAX` | >= 32767 | Maximum message number in message catalogues. |
| `NL_SETMAX` | `INT_MAX` | >= 255 | Maximum set number in message catalogues. |
| `NL_TEXTMAX` | `INT_MAX` | >= `_POSIX2_LINE_MAX` | Longest line in a message catalogue. |
| `SSIZE_MAX` | LP64: `INT64_MAX`; LP32: `INT32_MAX` | >= `_POSIX2_SSIZE_MAX` | Maximum value for an object of type `ssize_t`. |
