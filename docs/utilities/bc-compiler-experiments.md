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

Remaining #213 work includes matched GCC O2/O3 and thin/full/no-LTO
comparisons, supported instrumentation/sampling PGO, balanced training with
cold/error/interactive paths, independent holdout, source/compiler/profile
identity checks, optimization remarks, native code inspection, build cost,
RSS/code size, per-workload Howard ratios and startup. Missing or stale PGO
profiles must fail explicitly rather than silently selecting an unprofiled
build. No compiler performance gain or PGO acceptance is claimed here.
