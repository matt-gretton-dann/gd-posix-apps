# POSIX Applications

This repository is an implementation of the
[POSIX Utilities and Shell](https://pubs.opengroup.org/onlinepubs/9699919799/).

Why?  Because.  Mostly to understand the POSIX interface better, and also
because these are well-defined, often small projects which can be easily
implemented.  And those that aren't easy to implement are interesting.

## Copyright and Licensing

The code in this repository copyright Matthew Gretton-Dann, and licensed under
the [Apache License 2.0](./LICENSE).

There is also some usage of third-party libraries and code.  These are not shipped in this repo
but instead are downloaded in the configuration steps of the build.

See [Copyright and Licensing](./docs/copyright-and-licensing.md) for more details.

## Build & Test

The full build and test process is documented in the [Build and Test](./docs/build-and-test.md)
documents.

In summary: Ensure you have `git`, `cmake`, a sensible C/C++ compiler, and `Python` installed. Then
execute:

```sh
git clone https://github.com/matt-gretton-dann/gd-posix-apps
cd gd-posix-apps
cmake -Bbuild
cmake --build build
cmake --build build test
```
