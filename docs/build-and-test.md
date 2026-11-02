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

The following tools and libraries will be used if installed:

 * [Python](www.python.org) - at least version 3.6.  If not installed the testing will not be
   available.
 * [ClangFormat](https://clang.llvm.org/docs/ClangFormat.html).  If using version 9 or earlier then
   Python must also be installed.

### Configure dependencies

During the configuration process the build system will download and install the following
dependencies:

 * [fmt](https://fmt.dev/).  If the configure process decides that the system's C++ Standard Library
   does not support the `<format>` header we will use `fmt` instead.  This library will be shipped
   in the install package.
 * [Catch2](https://github.com/catchorg/Catch2).  If unit testing is enabled we use `Catch2` as the
   testing framework.

The pinned Catch2 v2 header uses `std::nothrow` without declaring it through
`<new>` on every include path. Test translation units include `<new>` before
Catch2 explicitly, so they work with standard libraries that remove incidental
transitive includes. The include-sorting policy preserves that order. This
does not require dependency edits or compiler-wide forced includes.

For more information on the licensing of these tools see the
[Copyright and Licensing](./copyright-and-licensing.md) page.

To disable the downloading of these third-party libraries pass
`-DALLOW_THIRD_PARTY_DOWNLOADS:BOOL=OFF` on the `cmake` configure command line.  If third-party
downloads are not allowed an attempt will be made to use versions of the libraries/tools already
installed on the machine.

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
| `BUILD_TESTING` | BOOL | ON | Build and register unit/integration tests, including supplemental-library tests. OFF requires no Catch2 test dependency. |
| `CMAKE_INSTALL_PREFIX` | STRING | /usr/local | Where to install things to. |
| `FORCE_SUPPLEMENTAL_LIBRARY` | BOOL | OFF | Force use of the [standard supplemental](./libgdsup/index.md) library even in cases where it is not needed. |
| `RUN_CLANG_FORMAT` | BOOL | OFF | Check source formatting during the build. |
| `RUN_CLANG_TIDY` | BOOL | OFF | Run clang-tidy during compilation. |
| `BC_MULTIPLY_SPLIT_POINT` | STRING | portable | Use the stable threshold 126, `auto` to calibrate native builds, or an explicit positive limb threshold. Cross-build `auto` uses the fixed fallback 128. |
| `IPO_MODE` | STRING | AUTO | Automatic IPO policy: preserve supported non-Debug detection (`AUTO`), disable project-managed IPO (`OFF`), or require support without fallback (`REQUIRED`). |
| `ENABLE_BC_COVERAGE` | BOOL | OFF | Instrument bc and its tests for line, branch and MC/DC coverage using a supported Clang/LLVM toolchain. |

For a production-only build use `-DBUILD_TESTING=OFF`. With a toolchain providing
the required standard facilities, `-DALLOW_THIRD_PARTY_DOWNLOADS=OFF` also avoids
fetching test dependencies. Other missing facilities can still require installed
supplemental dependencies. CI checks a nested testing-disabled configuration and
verifies that unit-test targets are absent.

Style checks are opt-in because their diagnostics and formatting rules vary
between tool versions. Compiler warnings remain enabled and are treated as errors.

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
cmake --build build
cmake --build build --target test
```

To test a build when CMake is using MS Build or MS Visual Studio:

```sh
cmake --build build
cmake --build build --target RUN_TESTS
```

:warning: The `test` and `RUN_TESTS` targets do not depend on the `all` target and so you need to
have explicitly ensured that the main build has been executed before running tests.

You can also invoke ctest directly by doing:

```sh
cd build
ctest
```


### Reproducible IPO controls

`IPO_MODE=AUTO` preserves the previous policy: supported non-Debug builds enable
CMake IPO, Debug skips detection, and unsupported toolchains use the generic
build. A multi-configuration generator retains the previous detection behavior;
no per-configuration policy is added. `IPO_MODE=OFF` skips the capability probe
and disables the project-managed IPO properties on utility executables and bc
library/test/component/tuner targets. `IPO_MODE=REQUIRED` probes even for Debug
and fails configuration if IPO is unavailable. Mode names are case-sensitive.

These settings control CMake's automatic IPO flags. Explicit compiler/link
flags remain caller inputs; other libraries can also inherit a caller-supplied
`CMAKE_INTERPROCEDURAL_OPTIMIZATION` setting. Use fresh build directories and
inspect actual compile/link commands when comparing compiler modes. In
particular, an explicit OFF request is not proof that manually supplied flags
contain no LTO. The default settings, optimization levels and portable kernels
are unchanged by this control.

Each successful configuration writes `gd-ipo-policy.json` with the selected
mode and automatic capability result. A failed reconfiguration removes a stale
report. This records CMake's selection; actual commands remain the evidence of
compiler behavior. The policy fixture tests supported/unsupported, Debug,
explicit opt-out, required-support failure and stale-report paths independently
of the host's LTO availability. This is groundwork for #213's matched compiler
and PGO experiments, not an established performance improvement.


The immutable-source bc builder accepts `--ipo-mode AUTO|OFF|REQUIRED` and
records the selected policy plus the report hash in `implementations.json`.
Explicit OFF/REQUIRED requests require a matching source-generated report;
a cached variable alone is insufficient. Older revisions remain buildable in
their existing default mode, labeled `legacy-auto` with the exact compiler
commands retained. They cannot satisfy an explicit newer policy request.
