# Header File Technical Notes

This document goes through each file making some notes about the implementation choices made.

## [gd/bits/defines.h](../../libgdsup/include/gd/bits/defines.h)

This is an internal header which provides useful macros used elsewhere.

## [gd/sys/stat.h](../../libgdsup/include/gd/sys/stat.h)

Currently just defines the `S_I[RWX](USR|GRP|OTH)` access macros for Windows 32.  See
[#156](https://github.com/matt-gretton-dann/gd-posix-apps/issues/156) for further info.

## [gd/bit.hh](../../libgdsup/include/gd/bit.hh)

Adds defines for `std::endian` and exposes them in the namespace `bit`.

## [gd/fcntl.h](../../libgdsup/include/gd/fcntl.h)

Currently just exposes an `open()` wrapper on Windows.

## [gd/filesystem.hh](../../libgsup/include/gd/filesystem.hh)

Exposes the `std::filesystem` library in the namespace `fs`.  On older compilers this may be from
the experimental filesystem library.

## [gd/format.hh](../../libgdsup/include/gd/format.hh)

Exposes the `std::format` library n the namespace `fmt`.  On old toolchains this will be the third-
part `{fmt}` library.

## [gd/libgen.h](../../libgdsup/include/gd/libgen.h)

Exposes the entirity of the `libgen.h` header.  We have implementations for `basename()` and
`dirname()` that are usable on all platforms.

## [gd/limits.h](../../libgdsup/include/gd/limits.h)

On Windows adds various defines that haven't been exposed.  We check that values meet required
standards as far as possible.  See [Limits](./limits.md) for details on individual limits.

## [gd/nl_types.h](../../libgdsup/include/gd/nl_types.h)

Adds support for message catalogues.  We overwrite the system provided functions as the message
catalogue format is not specified, so we use our own everywhere.

## [gd/stdlib.h](../../libgdsup/include/gd/stdlib.h)

Currently just a Windows wrapper around `mkstemp()`.  This makes use of Windows' `_mktemp_s()`
function which restricts us to 26 temporary files per root, per process.  This seems reasonable for
most uses - but low-priority issue
[#158](https://github.com/matt-gretton-dann/gd-posix-apps/issues/158) tracks this.

## [gd/unistd.h](../../libgdsup/include/gd/unistd.h)

Currently exposes Windows wrappers around `close()`, `read()`, `unlink()`, and `write()`.  For
`read()` and `write()` we ensure that we don't pass the underlying Win32 API functions more data
than they can handle.  We should consider doing the same on other platforms (see
[#159](https://github.com/matt-gretton-dann/gd-posix-apps/issues/159)).
