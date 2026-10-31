# Pinned bc comparator builds

`src/bc/scripts/build-bc-comparators.py` prepares source-built comparators for
#200. Use a fresh output directory; failed builds retain logs and an incomplete
manifest, and never publish an implementation configuration. Every command has
a finite deadline (900 seconds by default). Build success is not a correctness
or performance pass: run the checked corpus before collecting timings.

```sh
python3 src/bc/scripts/build-bc-comparators.py --output build-comparators \
  --implementation both --cc /absolute/path/to/cc
python3 src/bc/scripts/bench-bc.py \
  --config build-comparators/implementations.json --require-provenance \
  --output build-comparator-checks --preset quick --samples 1 --warmups 0
```

The builder pins:

- Howard 7.0.3, commit `c8dabac65d39fd527a9264a5e505a99b1d8df3d8` from
  [upstream](https://github.com/gavinhoward/bc/tree/c8dabac65d39fd527a9264a5e505a99b1d8df3d8).
  The tag must resolve to that commit. A local Git cache can be supplied with
  `--howard-repository`; cloning uses committed source, excluding local edits.
- GNU 1.08.2 from the [GNU release archive](https://ftp.gnu.org/gnu/bc/), SHA-256
  `ae470fec429775653e042015edc928d07c8c3b2fc59765172a330d3d87785f86`.
  `--gnu-archive` permits an offline archive with the same mandatory checksum.

On POSIX hosts both builds explicitly use the chosen C compiler, `-O3 -DNDEBUG`,
empty incoming CPPFLAGS/LDFLAGS, and no configuration site file. BC_/DC_ options
are removed from the build environment. Howard uses upstream optimization level
3 with NLS, history, dc, manual generation, generated tests and stripping
disabled. Its extra math remains enabled and its upstream Karatsuba threshold
is 32 limbs. GNU disables NLS/readline and builds only `lib` and `bc`, avoiding a
Texinfo dependency for unused documentation. Their exact expanded Makefile
CC/CFLAGS/CPPFLAGS/LDFLAGS values are recorded rather than approximated.

For Windows run from an x64 Visual Studio developer command prompt:

```bat
python src\bc\scripts\build-bc-comparators.py --output build-comparators --cc cl
```

The native Howard recipe uses upstream `vs/bc.sln`, Release/x64, with an explicitly selected installed x64 MSVC toolset.
The recipe discovers the newest installed x64 toolset, or uses `--msvc-toolset`
when specified, and records the selection. This can override the project's
original v143 default on newer Visual Studio installations. It selects the
recorded compiler executable through CLToolPath/CLToolExe and
preserves MSBuild's actual CL command tracking records. Upstream Windows defaults
include both calculators, extra math and history, with NLS disabled; these are
not the POSIX history-disabled configuration. See the pinned
[upstream build guide](https://github.com/gavinhoward/bc/blob/7.0.3/manuals/build.md).
The GNU configure recipe is explicitly unavailable on native Windows; use the
native Howard comparison there. Native CI checks the supported recipes and the
32 quick output oracles; these one-sample checks make no speed claim.

`implementations.json` can be passed directly to the checked benchmark runner.
Add separately built GD base/candidate entries for a competitive experiment.
`build-manifest.json` records platform, builder checksum, command argv/status and
log paths. Each implementation records source revision/checksum, compiler
version/path/checksum, actual flags/configuration, binary version/checksum and
tuning defaults. Keep the source trees, command logs and manifest with results.
An explicit compiler path plus its recorded checksum identifies that local
compiler; reproducing it elsewhere still requires provisioning that same
compiler and standard C runtime. Record the host/runtime and SDK as well; the
builder does not provision a complete reproducible operating-system image.

Apple's `/usr/bin/bc` must remain a distinct `role: installed` entry with unknown
source/build fields left unknown. It cannot become a strict controlled reference
by borrowing metadata from the source-built Howard. Release GD provenance must
include its exact revision, compiler/library, CMake flags/options and generated
multiplication threshold. Profiling and coverage builds are separate and must
be marked instrumented, never included in final timing configurations.

Use at least the default nine paired observations plus warm-up for timings,
A/A runs to assess noise, and distinct corpus seeds for training and holdouts.
Do not compare absolute medians from different hosts as a regression gate.
#216 owns same-VM base/candidate orchestration on Linux, macOS and Windows;
component measurements and profiling remain separate from process timings.
