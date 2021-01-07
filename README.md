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

## Build Process

### Dependencies

### Pre-configure dependencies

The build process requires the following tools and libraries to be installed
before building:

 * [Git](https://git-scm.com/).
 * [CMake](www.cmake.org) - at least version 3.15.
 * A C/C++ Compiler
 * A Make system compatible with CMake.
 * [Python](www.python.org) - at least version 3.8.

### Configure dependencies

During the configuration process the build system will download and install
the following dependencies:

 * TBD

### Building

```sh
git clone https://github.com/matt-gretton-dann/gd-posix-apps.git
cd gd-posix-apps
cmake -Bbuild
cmake --build build
```

### Testing

To test a build do:

```sh
cmake --build build --target test
```

