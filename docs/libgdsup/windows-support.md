# Supporting Windows

## Function Wrappers

The Win32 API provides some POSIX functionality - although not necessarily in the standard header.
Notably most IO is in the `<io.h>` header.

It also warns if you use the official name (`open()` say), complaining that the name is non-standard
and you should use the renamed version (e.g. `_open()`).  You can turn these warnings off by
declaring a macro.

However, this turns out not to be enough.

Windows also doesn't define some types.  Notably, `ssize_t`.  This means that some functions (
including `read()` and `write()`) take `size_t` operands but return `int` values.  Meaning that the
restriction on the maximum sensible value for a `size_t` object is `INT_MAX`.

We therefore make use of the `_CRT_DECLARE_NONSTDC_NAMES` macro setting it to 0:

```c
#define _CRT_DECLARE_NONSTDC_NAMES 0
```

This still causes the functions prefixed by `_` to be declared, but not the 'normal' POSIX names.

This gives us the ability to declare `ssize_t` and then marshal the calls to `read()` and `write()`
to not pass illegal values to the Win32 implementations.

## `ssize_t` underlying type

Following on from the above the decision has been made to keep `ssize_t` to be the same size as
`size_t`.  That is `ssize_t` is a signed version of `size_t`.  On Windows another alternative would
be to just declare it as `int` as that matches the functions that take/return `ssize_t` values.

However, I think that most people assume `sizeof(ssize_t) == sizeof(size_t)` so have made the first
choice using the principle of least surprise.  However, it may need to be revisited at a later date
considering usage patterns in Windows.

## File Permissions, Users & Groups, Sharing

In the POSIX `<sys/stat.h>` header Windows doesn't define read/write permissions on a user, group,
others basis.  Instead it provides it for the user only.

Currently we ignore the group and others settings mapping them to the user.

[Issue #156](https://github.com/matt-gretton-dann/gd-posix-apps/issues/156) tracks fixing this
issue.

## User and group IDs.

In POISX user & group IDs are simple integer types.  Unfortunately Windows doesn't and instead uses
a full Security Descriptor.  We currently don't have a fix for this.

[Issue #157](https://github.com/matt-gretton-dann/gd-posix-apps/issues/157) tracks fixing this.

## `mkstemp()`

The mkstemp() wrapper may be too basic as it only allows 26 files per root/process.

 [Issue #158](https://github.com/matt-gretton-dann/gd-posix-apps/issues/158) tracks fixing this.
