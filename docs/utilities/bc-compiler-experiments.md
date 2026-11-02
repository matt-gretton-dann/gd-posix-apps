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

Remaining #213 work includes matched Clang/GCC O2/O3 and thin/full/no-LTO
comparisons, supported instrumentation/sampling PGO, balanced training with
cold/error/interactive paths, independent holdout, source/compiler/profile
identity checks, optimization remarks, native code inspection, build cost,
RSS/code size, per-workload Howard ratios and startup. Missing or stale PGO
profiles must fail explicitly rather than silently selecting an unprofiled
build. No compiler performance gain or PGO acceptance is claimed here.
