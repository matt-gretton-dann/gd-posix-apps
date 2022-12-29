# Undefined and Unspecified Behaviour Descriptions

This document details the behaviours in the POSIX utilties that are described as 'unespecified' or
'undefined'.

The general principle is to be 'noisy' about such behaviour, usually by at least a warning to
standard error, and oftern by a non-zero exit code.

## General behaviours

For utilities that take no option arguments (e.g. `basename`, `printf`), unless otherwise stated by
the standard (see `echo`), we treat arguments starting `-` as unknown options.
[#173](https://github.com/matt-gretton-dann/gd-posix-apps/issues/173)

For arguments and operands that are signed integers we accept numbers in the range \[-2147483647,
2147483647]. Similartly for unsigned integers we accept numbers in the range \[0, 2147483647].

We support `-` as representing standard input whenever that is a valid operand.

## Specific Utilities

### asa

Explicitly unspecified behaviour:

* When encountering an unrecognised character at the start of a line we treat it as a <space>.
  This follows the recommendation of the standard. TBD: Warn?

Implementation behaviours:

* If two files are given to asa, and the second file starts with a `+` we treat that as issuing a
  `\r` and not as equivalent to `<space>`
* If two files are given to asa, and the second file starts with a `<space>` we carry on from the
  end of the last line of the previous file.

These behaviours basically say that:

```sh
asa file1 file2
```

is equivalent to:

```sh
cat file1 file2 | asa -
```

### basename

`basename` is muddied by the underlying needs of the target and host platforms.

Explicitly unspecified behaviours:

* `basename ''` returns `.`.
* `basename //` may return either `/` or `//`.

### awk

#### Undefined Behaviour

* For Octal escape sequences that are the NUL-character `'\0'` we report an error, but include the
  NUL-character in the string or ERE.

#### Implementation defined behaviour.

For numbers we accept both hex integer and floating-point forms.

### bc

Full documentation on how `bc` is implemented is in the [implementation guide](./bc.md).

Limits are described in the following table. Note that the targets are different for 32-bit and
64-bit target platforms.

| Limit name      | POSIX Min. | 32-bit target | 64-bit target | Description                                                            |
|:----------------|:-----------|:--------------|:--------------|:-----------------------------------------------------------------------|
| `BC_STRING_MAX` |            | Memory        | Memory        | Maximum length of string literal.                                      |
| `BC_DIM_MAX`    |            | 9999          | 9999999999    | Maximum dimension of an array.                                         |
| `BC_SCALE_MAX`  |            | 9999          | 999999999     | Maximum scale of a number (digits after decimal point).                |
| `BC_BASE_MAX`   |            | 9999          | 999999999     | Maximum output base.                                                   |
| `BC_LENGTH_MAX` |            | 9999          | 9999999999    | Maximum number of digits in a number (before and after decimal point). |

`BC_LENGTH_MAX` is not specified by POSIX. We should actual have it as 9999*4 for 32-bit systems or
999999999*9 for 64-bit systems see
[#184](https://github.com/matt-gretton-dann/gd-posix-apps/issues/184).

We reject numbers which are longer than one digit that do contain digits greater than or equal to
the current ibase. We accept single digit numbers where the digit is greater than the base
everywhere (and not just when assigning to `ibase` or `obase`).

We treat `=-` as an error.

The power operator can only raise numbers to integer powers. So `2^3` and `4^-5` are valid but
`1^0.6` is not.

Invoking functions with a different number of parameters compared to that which it was defined with
is an error and causes the program to terminate.

Errors in input/execution cause `bc` to terminate immediately.

### cat

Explicitly unspecified behaviour:

* Files are opened with their default buffering (normally full for files, and line for standard
  input). Use `-u` to get no buffering. Specifying `-u` also sets standard output to unbuffered.

### cksum

No unspecified behaviours.

### dirname

`dirname` is muddied by the underlying needs of the target and host platforms.

Explicitly unspecified behaviours:

* `dirname //` may return either `/` or `//`.

### echo

Implementation of `echo` follows the XSI rules, and so all operands are taken to be echoed ignoring
the usual utility conventions.

### expr

Explicitly unspecified behaviours:

* Pattern matching. `expr` adds an implicit '^' to the regular expression being matched. POSIX says
  that it is unspecified whether ^ at the start of the user supplied regular expression argument
  should be treated specially or not. We treat it specially. So `expr foo : ^bar` is the same as
  `expr foo : bar`. This may be reviewed:
  [#176](https://github.com/matt-gretton-dann/gd-posix-apps/issues/176)
* We treat `length`, `substr`, `index`, or `match` as normal operands and do not give them any
  special meaning.

Implementation behaviours:

* Input numbers are in the range \[-2147483647, 2147483647].
* Output numbers are in the range \[-2147483648, 2147483647]. We do warn on overflow.
* This behaviour may change in the future as the standard does not make it clear if we have to be
  infinitely precise: [#175](https://github.com/matt-gretton-dann/gd-posix-apps/issues/175)
* Currently `EXPR_NEST_MAX` is ignored (this is a bug):
  ([#174](https://github.com/matt-gretton-dann/gd-posix-apps/issues/174)).

### false

As seems to be common practice, `false` silently ignores all command-line arguments.

### gencat

Explicitly unspecified behaviours:

* See [message-catalogues.md](../file-formats/message-catalogues.md) for description of the message
  catalogue format.

### printf

Optional behaviours:

* We do not support `a`, `A`, `e`, `E`, `f`, `F`, `g`, `G` conversion specifiers.
  [#179](https://github.com/matt-gretton-dann/gd-posix-apps/issues/179)
* We do not implement suffixed integer constants (e.g. `10U`).
  [#180](https://github.com/matt-gretton-dann/gd-posix-apps/issues/180)
* We do not provide support for specifying width or precision options as '*'.

Explicitly 'unspecified' behaviours:

* \ escape sequences not specified by the standard are considered an error.
* If the format string contains no format specifiers and arguments are provided then we report an
  error. We also do a conversion of the format string with NULL/0 inputs. A format string with
  no format specifiers and no arguments on the command-line is no an error however.
* Unrecognised format specifier sequences are considered an error.
* The result of `printf %c ''` as an empty string (the standard also allows a nul-character).

Implementation behaviours:

* We handle integers in the range \[-2147483647, 2147483647] for both input and output.
* We do not treat `\8` or `\9` as valid octal sequences.
* `printf "%.4b\\n" "Hello\\c World" "foo"` will output `Hell`. That is even though it is not
  output the `\c` escape will be noted and acted upon.

### true

As seems to be common practice, `true` silently ignores all command-line arguments.
