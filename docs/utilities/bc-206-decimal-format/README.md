# Decimal limb formatting through a fixed buffer

Refs #206 and #197. Exact accepted baseline 9669ef73bc00e5b89f3fb00e2867a8f8ed9c4d32
and candidate d0947b5f40ca95a059fe30193168d7bf4c217e5e; ordinary conversion-out
component mode, actual source/flags/compiler/library/binary hashes in recipes.

Classic-locale formatting uses one checked, pre-reserved coefficient string
and one reusable stack buffer of a limb width. The first limb uses to_chars;
interior limbs have exactly the decimal storage width, including zero padding.
This does not retain global memory or make complete formatting constant-space.
Nonclassic global locales retain the historical temporary stream fallback,
independently of destination locale. Existing scale and wrapping paths are shared.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, fixed split 126,
extensions ON, profiling/coverage OFF. Full 400 and held-out 200 invocations
pass independent decimal text/continuation-length checksum, status, stderr and
deadline checks. One checked warmup and nine shuffled paired rounds; 2000
bootstrap resamples. No local builds, tests or profiling overlap timing.
Shared-host contention/frequency/thermals remain uncontrolled. Separate controls
are auxiliary and never subtracted. Small-loop compiler folding is possible.

Ratios are approximately 0.54 for one-digit output, 0.26 for 1000 decimal
digits, and 0.20 at 100000 digits. The held-out 120000-digit ratio is 0.199.
No measured interval has a lower endpoint above 1.05. These are scoped local
component results; native three-platform and end-to-end acceptance is pending.

Release passes 199 cases/691577 assertions and eight CLI groups. Six new
compatibility cases pass on the old formatter before implementation. The
extensions-OFF ASan/UBSan run passes 29 output cases/19291 assertions and
fifteen VM CLI groups. Checks include exact interior zeros, custom global
punctuation, destination locale, scales, signs, stream width and continuation.

## full

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b10-1d-x50000` | 0.537 [0.535,0.538] |
| `output-b10-9d-x25000` | 0.538 [0.538,0.540] |
| `output-b10-10d-x25000` | 0.444 [0.443,0.449] |
| `output-b10-26d-x10000` | 0.513 [0.510,0.521] |
| `output-b10-27d-x10000` | 0.516 [0.515,0.520] |
| `output-b10-28d-x10000` | 0.471 [0.469,0.477] |
| `output-b10-29d-x10000` | 0.480 [0.468,0.485] |
| `output-b10-35d-x10000` | 0.472 [0.465,0.476] |
| `output-b10-36d-x10000` | 0.494 [0.493,0.497] |
| `output-b10-37d-x10000` | 0.439 [0.437,0.442] |
| `output-b10-68d-x5000` | 0.400 [0.398,0.403] |
| `output-b10-69d-x5000` | 0.423 [0.418,0.432] |
| `output-b10-70d-x5000` | 0.424 [0.416,0.428] |
| `output-b10-128d-x2500` | 0.368 [0.364,0.370] |
| `output-b10-1000d-x2000` | 0.256 [0.255,0.267] |
| `output-b10-10000d-x200` | 0.206 [0.205,0.210] |
| `output-b10-100000d-x100` | 0.205 [0.204,0.206] |
| `control-1d` | 1.000 [0.994,1.008] |
| `control-1000d` | 0.994 [0.976,1.013] |
| `control-10000d` | 1.001 [0.903,1.151] |

## holdout

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b10-3d-x50000` | 0.546 [0.544,0.557] |
| `output-b10-67d-x5000` | 0.395 [0.389,0.399] |
| `output-b10-71d-x5000` | 0.424 [0.415,0.427] |
| `output-b10-257d-x500` | 0.345 [0.338,0.359] |
| `output-b10-2048d-x1000` | 0.229 [0.228,0.230] |
| `output-b10-20000d-x200` | 0.200 [0.197,0.202] |
| `output-b10-120000d-x100` | 0.199 [0.197,0.206] |
| `control-1d` | 1.003 [1.000,1.025] |
| `control-1000d` | 0.994 [0.988,0.997] |
| `control-10000d` | 1.000 [0.950,1.030] |

