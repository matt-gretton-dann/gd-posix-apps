# Building and testing GD-Posix-Apps

This page details the build process for GD-Posix-Apps.

## Dependencies

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

For more information on the licensing of these tools see the
[Copyright and Licensing](./copyright-and-licensing.md) page.

## Building

### Configuration

```sh
git clone https://github.com/matt-gretton-dann/gd-posix-apps.git
cd gd-posix-apps
cmake -Bbuild
```

### CMake options

The following options can be passed to CMake to change the build behaviour:

| Cmake Option | Type | Default | Meaning |
| :----------- | :--- | :------ | :------ |
| `ALLOW_THIRD_PARTY_DOWNLOADS` | BOOL | ON | Are we allowed to download third-party tools?  If set to no, may cause build-failures on some systems. |
| `BUILD_TESTING` | BOOL | ON | Is testing enabled? |
| `CMAKE_INSTALL_PREFIX` | STRING | /usr/local | Where to install things to. |
| `FORCE_SUPPLEMENTAL_LIBRARY` | BOOL | OFF | Force use of the [standard supplemental](./libgdsup/index.md) library even in cases where it is not needed. |

To specify these options either use `ccmake` to set them graphically or add `-D<OPT>:<TYPE>=<VALUE>`
to the `cmake` command line.  For instance:

```sh
cmake -Bbuild -DFORCE_SUPPLEMENTAL_LIBRARY:BOOL=ON
```

### Building

To build everything:

```sh
cmake --build build
```

or to specify a target:

```sh
cmake --build build --target <TGT>
```

This will cause CMake to invoke the appropriate build system.  You can also invoke the build system
directly.

### Testing

To test a build when CMake uses Ninja or Makefiles as its target (default for macOS and Linux):

```sh
cmake --build build --target test
```

To test a build when CMake is using MS Build or MS Visual Studio:

```sh
cmake --build build --target RUN_TESTS
```

You can also invoke ctest directly by doing:

```sh
cd build
ctest
```
