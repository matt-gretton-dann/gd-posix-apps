# Compiler and profile experiment controls

Refs #213, #200 and #197. The compiler/PGO performance grid remains incomplete;
these controls make its initial comparisons explicit and reproducible.

`IPO_MODE=AUTO` preserves the supported non-Debug automatic default.
`IPO_MODE=OFF` disables the automatic IPO properties on project-managed
utility and bc targets. `IPO_MODE=REQUIRED` fails configuration if CMake's
joint C/CXX IPO capability probe fails, including an explicit Debug request.
Unsupported AUTO builds retain the generic fallback. Explicit compiler/link
flags remain separate caller inputs; mode selection is not a claim about
those manually supplied flags or about target-specific instruction selection.

Use fresh immutable-source build recipes with fixed multiplication thresholds:

```sh
python3 src/bc/scripts/build-gd-bc.py --revision "$REVISION" \
  --split-point 126 --ipo-mode OFF --output build/compiler-no-ipo
```

Set REVISION to the full forty-digit source commit containing the controls.
The builder records the requested/applied policy, original JSON bytes and SHA-256,
compiler identities, flags and actual compilation database where available.
Native Visual Studio build logs remain required when the generator has no
compilation database. A cache key can be ignored by an older CMake source, so
explicit policies require the matching source-generated result. Missing,
malformed, stale or contradictory reports fail before publishing the executable
configuration. Older default builds remain available as `legacy-auto`, not as
verified explicit opt-outs. A failed CMake reconfiguration removes the previous
successful policy report.

Local validation uses eleven deterministic policy tests, eleven builder
provenance tests, real Release AUTO/OFF builds and 36 benchmark checks for each
build, including 56 checked quick cases. Actual local commands show eleven bc
compilation units with thin LTO in AUTO and none in OFF, and the corresponding
link policy. REQUIRED configures successfully on this supported compiler;
unsupported REQUIRED and stale-report removal are checked by controlled
capability fixtures. Native CI also configures the production-only OFF path.
These are correctness/configuration observations, not speed measurements.

## Apple Clang optimization-level comparison

Both binaries used source `72485e3490bc22a91a1d63cc057e7da73c7d019e`,
Apple Clang 21.0.0 (clang-2100.3.34.2), macOS 27.2 arm64, CMake 4.4.5,
Ninja 1.13.2, Release, and a fixed multiplication split point of 126.
The actual VM compile commands used ThinLTO in both builds. Each binary
passed the 19 extensions-enabled VM CLI tests.

```sh
for level in 2 3; do
  cmake -S . -B "build-o$level" -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DBUILD_TESTING=OFF -DIPO_MODE=REQUIRED -DBC_MULTIPLY_SPLIT_POINT=126 \
    "-DCMAKE_C_FLAGS_RELEASE=-O$level -DNDEBUG" \
    "-DCMAKE_CXX_FLAGS_RELEASE=-O$level -DNDEBUG"
  cmake --build "build-o$level" --target bc -j 3
done
```

Configure `bench-bc.py` with those two binaries as `gd-o2` and `gd-o3`
and the source-built Howard 7.0.3 comparator from revision
`c8dabac65d39fd527a9264a5e505a99b1d8df3d8`. See the
[comparator build recipe](bc-comparator-builds.md).
Run the arithmetic `full` and `holdout` presets independently with
`--reference gd-o3 --samples 9 --warmups 1 --seed 197200 --corpus-seed 197200`.
The harness checked 1,980 outputs per preset, including warmups.

| Variant | Configure wall seconds | Build wall seconds | Executable bytes |
| --- | ---: | ---: | ---: |
| `-O2` | 2.77 | 8.78 | 567744 |
| `-O3` | 2.57 | 8.68 | 567232 |

The only nominal paired interval entirely beyond a 5% difference was
`binary_input_100000d_x3` in the full preset. The `-O2` runtime was
1.068 times the `-O3` runtime, with a 95% interval of [1.051, 1.083].
Startup intervals included parity in both presets. These single-host
measurements had no separate A/A control and use pointwise intervals.
The build times are single observations. The release default remains
`-O3`.

The [per-workload ratios](bc-compiler-apple-optimization/ratios.csv)
include each optimization level against Howard and `-O2` against `-O3`.
The [full](bc-compiler-apple-optimization/full.jsonl.gz) and
[holdout](bc-compiler-apple-optimization/holdout.jsonl.gz) archives retain
the original checked samples.

## Apple Clang LTO comparison

Repeat the build at the same source and split point for each combination of
`-O2` or `-O3` and no LTO, ThinLTO or full LTO. Use a fresh build directory
for each configuration. For ThinLTO use the recipe above. For no LTO use
`IPO_MODE=OFF`. For full LTO use `IPO_MODE=OFF` and append `-flto=full`
to both Release flags. The latter disables CMake's automatic IPO and
lets the explicit compiler flag select full LTO. Check the actual compile
and link commands before interpreting the results.

The two rebuilt ThinLTO executables were byte-identical to the first pair.
All six builds passed the 19 extensions-enabled VM CLI tests. Run the
same two arithmetic presets with seven interleaved roles, including the
same Howard binary and `-O3` ThinLTO as reference.

| Level | LTO | Configure wall seconds | Build wall seconds | Executable bytes |
| --- | --- | ---: | ---: | ---: |
| `-O2` | None | 2.37 | 9.15 | 746896 |
| `-O2` | Thin | 2.57 | 8.97 | 567744 |
| `-O2` | Full | 2.26 | 10.10 | 533104 |
| `-O3` | None | 2.42 | 9.60 | 754752 |
| `-O3` | Thin | 2.61 | 9.46 | 567232 |
| `-O3` | Full | 2.25 | 9.53 | 531872 |

In the full preset, the `-O3` full-LTO binary ran
`factorial_5000_x5` at 1.398 times the reference runtime with a nominal
95% interval of [1.390, 1.399]. Without LTO, the `-O3` build ran
`file_statements_20000` at 1.125 times the reference runtime with an
interval of [1.124, 1.129].

The held-out `factorial_5625_x5` case showed a similar full-LTO cost
at 1.408 [1.400, 1.411]. The held-out `file_statements_17500` case
without LTO ran at 1.120 [1.103, 1.127]. Both comparisons use the
`-O3` builds. Full LTO made the executable smaller, and ThinLTO
remains the default for this configuration. These single-host
measurements had no separate A/A control and use pointwise intervals.

The [full](bc-compiler-apple-optimization/lto-full.jsonl.gz) and
[holdout](bc-compiler-apple-optimization/lto-holdout.jsonl.gz) archives
contain 9,240 checked observations in total, including warmups.
The [paired ratios](bc-compiler-apple-optimization/lto-ratios.csv)
include comparisons with the ThinLTO reference and with Howard.

## Apple Clang VM profile-guided optimization

Use the same source, compiler, split point and `-O3` ThinLTO recipe as the
compiler matrix. Configure the training build with
`-DENABLE_BC_VM_PGO_GENERATE=ON` and set `LLVM_PROFILE_FILE` to a fresh
directory with a `%p.profraw` filename. Train on the arithmetic `quick`
preset with corpus seed 21301, one warmup and one measured iteration per
workload. Also run `test-cli.py --extensions ON`,
`test-signal-delivery.py` and `test-math-library-cli.py` to include error
paths, interrupt recovery and portable math. Merge the 410 raw profiles
with the matching `xcrun llvm-profdata`. The merged IR profile covered
402 functions and had SHA-256
`0f168c6369c63ec140216cec7f438f1dc9cc81a07a3cb7fa9c38fb1d61620d61`.

Configure a fresh build with `-DBC_VM_PGO_PROFILE=/path/to/training.profdata`
and no profile generation. The compilation database showed profile use
only in `vm.cc`. The resulting binary passed 71 local checks, with one
optional external math test skipped. The training and profile-use builds
took 9.35 and 9.82 seconds respectively, with one build per variant.
The profiled binary was 649040 bytes versus 567232 for the unprofiled
ThinLTO control.

Use the full and holdout arithmetic presets with the same Howard binary,
reference `base` for unprofiled ThinLTO, and role `pgo` for the profiled
build. Use nine measured pairs and one warmup. The quick training
programs and held-out arithmetic programs shared one exact program, the
startup probe. Startup is reported separately.

The full preset showed faster loops and calls at 0.763 times control
runtime in each case. The large factorial workload ran at 1.385
[1.331, 1.392], and `divide_multilimb_900d_450d_s100_x1000` at 1.296
[1.260, 1.329]. The intervals are nominal pointwise 95% intervals on one
host without a separate A/A control.

The held-out loop and call ratios were 0.764 and 0.765. Large factorial
ran at 1.399 [1.397, 1.404] and the corresponding multi-limb division
case at 1.169 [1.164, 1.198]. A different held-out multi-limb division
case ran at 0.779 [0.776, 0.792]. Startup intervals included parity in
both presets. The workload tradeoff leaves this profile opt-in.

The [training corpus hashes](bc-compiler-apple-optimization/pgo-training.json)
identify the quick inputs. The [full](bc-compiler-apple-optimization/pgo-full.jsonl.gz)
and [holdout](bc-compiler-apple-optimization/pgo-holdout.jsonl.gz) archives
retain 3,960 checked observations, including warmups. The
[paired ratios](bc-compiler-apple-optimization/pgo-ratios.csv) retain both
builds against Howard and PGO against the unprofiled control.

Remaining #213 work includes matched GCC O2/O3 and LTO comparisons,
supported instrumentation/sampling PGO, balanced training with
cold/error/interactive paths, independent holdout, source/compiler/profile
identity checks, optimization remarks, native code inspection, build cost,
RSS/code size, per-workload Howard ratios and startup. Missing or stale PGO
profiles must fail explicitly rather than silently selecting an unprofiled
build. No compiler performance gain or PGO acceptance is claimed here.

### Apple Clang native target on M1

On macOS arm64, Apple Clang 21.0.0 (clang-2100.3.34.2) selected
`apple-m1` both by default and with `-mcpu=native`, as reported by
`clang++ -###`. At revision `4934a4e0`, configure three independent Ninja
Release builds with `BUILD_TESTING=OFF` and the default extension and
ThinLTO settings. Set `CMAKE_CXX_FLAGS=-mcpu=native` in one build and
leave it empty in both portable controls. Build the `bc` target and
verify the flag in the native compilation database.

All three executables were 568,080 bytes and had SHA-256
`466d774e457249996f8de523d8ee468271386588689f597273f055708216baf6`.
Each passed 36 CLI checks and 18 math checks with one optional math
check skipped. Build times were 11.43 s and 11.04 s for the portable
controls and 11.02 s for the native build, one measurement per build.
The matching binaries give no reason to add a target flag for this
compiler and host. Other CPU and compiler combinations need separate
measurements.
