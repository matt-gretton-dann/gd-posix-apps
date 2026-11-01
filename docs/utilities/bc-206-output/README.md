# Bounded numeric output writes

Exact baseline `b15806ac7470eee9cfecf9e598047109bef32a62`; integrated
batched source `e88cafbd7551f8faf331367e3b044aab451e4abc`. Actual source,
compiler/library identity, flags, commands and binary hashes are in the
release/component recipes. This stage retains the corrected GCC decoder
parent; it does not include the held array implementation.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, extensions ON, fixed
split point 126; profiling/coverage OFF. All 2760 full and 460 held-out
component invocations pass independent integer radix and exact continuation-
length checks, output/status/stderr checks and deadlines. Full/held-out
use different seeds and sizes, one checked warm-up and nine shuffled paired
rounds with 2000 bootstrap resamples. No local build/test/profile overlaps
timings. Host load, frequency and thermal state remain uncontrolled.

Internal intervals include formatting, buffer reset and consumer work.
Startup and fixture parsing are outside these component intervals. Controls
remain separate and are never subtracted. Positive integer timing fixtures
do not represent all fractional/sign/stream-error regimes. Independent
all-trait tests cover output positions, fractions/padding and width behavior.

The test-first stream-buffer oracle fails at 182 block operations for a
180-digit coefficient on all three traits. The implementation emits bounded
whole-coefficient spans and views of existing wrapped text. It preserves
the locale-aware limb formatter, continuation rules, and first-character
stream width. Release: 181 cases / 628045 assertions plus eight CLI/tool
groups; after carrying the GCC parent, OFF ASan/UBSan: 140 arithmetic/VM
cases / 443827 assertions and 15 CLI checks, plus eleven profiling checks.

No measured interval is wholly above the provisional 1.05 regression
threshold. Decimal component ratios range from approximately 0.90 for one
digit to 0.28 for 10000 digits. Generic nondecimal conversion retains its
arithmetic algorithm; view emission alone need not improve its dominant
division cost. All losing/noisy intervals and controls remain below and in
the raw data. These local component results do not establish native adoption
or complete #206 acceptance.

## Full intervals

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b2-1d-x10000` | 0.977 [0.971,0.989] |
| `output-b2-9d-x5000` | 0.957 [0.950,0.961] |
| `output-b2-10d-x5000` | 0.958 [0.957,0.963] |
| `output-b2-68d-x1000` | 0.987 [0.984,0.991] |
| `output-b2-69d-x1000` | 0.979 [0.970,0.983] |
| `output-b2-70d-x1000` | 0.980 [0.978,0.989] |
| `output-b2-128d-x500` | 0.987 [0.983,0.989] |
| `output-b2-1000d-x20` | 1.000 [0.996,1.006] |
| `output-b2-10000d-x1` | 1.000 [0.999,1.023] |
| `output-b3-1d-x10000` | 0.994 [0.987,1.000] |
| `output-b3-9d-x5000` | 0.984 [0.967,0.997] |
| `output-b3-10d-x5000` | 0.978 [0.975,0.982] |
| `output-b3-68d-x1000` | 0.987 [0.981,0.991] |
| `output-b3-69d-x1000` | 0.982 [0.967,0.988] |
| `output-b3-70d-x1000` | 0.985 [0.980,0.990] |
| `output-b3-128d-x500` | 0.988 [0.985,0.998] |
| `output-b3-1000d-x20` | 0.999 [0.991,1.000] |
| `output-b3-10000d-x1` | 1.000 [0.989,1.004] |
| `output-b4-1d-x10000` | 0.995 [0.993,1.000] |
| `output-b4-9d-x5000` | 0.978 [0.970,0.995] |
| `output-b4-10d-x5000` | 0.993 [0.985,1.003] |
| `output-b4-68d-x1000` | 0.984 [0.972,0.993] |
| `output-b4-69d-x1000` | 0.978 [0.970,0.984] |
| `output-b4-70d-x1000` | 0.982 [0.973,0.992] |
| `output-b4-128d-x500` | 0.987 [0.976,0.989] |
| `output-b4-1000d-x20` | 1.000 [0.998,1.002] |
| `output-b4-10000d-x1` | 0.999 [0.994,1.010] |
| `output-b5-1d-x10000` | 0.995 [0.992,1.000] |
| `output-b5-9d-x5000` | 0.975 [0.972,0.976] |
| `output-b5-10d-x5000` | 0.980 [0.977,1.003] |
| `output-b5-68d-x1000` | 0.977 [0.971,0.992] |
| `output-b5-69d-x1000` | 0.976 [0.968,0.984] |
| `output-b5-70d-x1000` | 0.984 [0.978,0.986] |
| `output-b5-128d-x500` | 0.986 [0.982,0.991] |
| `output-b5-1000d-x20` | 0.998 [0.996,1.001] |
| `output-b5-10000d-x1` | 1.000 [0.997,1.007] |
| `output-b6-1d-x10000` | 0.996 [0.991,1.000] |
| `output-b6-9d-x5000` | 0.991 [0.982,0.996] |
| `output-b6-10d-x5000` | 0.982 [0.980,0.987] |
| `output-b6-68d-x1000` | 0.989 [0.978,0.992] |
| `output-b6-69d-x1000` | 0.982 [0.977,0.993] |
| `output-b6-70d-x1000` | 0.985 [0.969,0.991] |
| `output-b6-128d-x500` | 0.990 [0.980,0.992] |
| `output-b6-1000d-x20` | 1.000 [0.996,1.003] |
| `output-b6-10000d-x1` | 1.000 [0.997,1.027] |
| `output-b7-1d-x10000` | 0.998 [0.988,1.001] |
| `output-b7-9d-x5000` | 0.985 [0.980,0.995] |
| `output-b7-10d-x5000` | 0.984 [0.982,0.992] |
| `output-b7-68d-x1000` | 0.980 [0.968,0.989] |
| `output-b7-69d-x1000` | 0.982 [0.974,0.990] |
| `output-b7-70d-x1000` | 0.987 [0.982,1.001] |
| `output-b7-128d-x500` | 0.993 [0.987,1.002] |
| `output-b7-1000d-x20` | 0.998 [0.885,1.001] |
| `output-b7-10000d-x1` | 0.999 [0.999,1.001] |
| `output-b8-1d-x10000` | 0.999 [0.991,1.075] |
| `output-b8-9d-x5000` | 0.984 [0.970,0.991] |
| `output-b8-10d-x5000` | 0.980 [0.977,0.992] |
| `output-b8-68d-x1000` | 0.993 [0.991,0.997] |
| `output-b8-69d-x1000` | 0.988 [0.980,0.999] |
| `output-b8-70d-x1000` | 0.982 [0.973,0.992] |
| `output-b8-128d-x500` | 0.993 [0.986,0.999] |
| `output-b8-1000d-x20` | 0.999 [0.996,1.000] |
| `output-b8-10000d-x1` | 0.999 [0.997,1.002] |
| `output-b9-1d-x10000` | 1.003 [0.991,1.071] |
| `output-b9-9d-x5000` | 0.989 [0.981,0.993] |
| `output-b9-10d-x5000` | 0.982 [0.980,0.982] |
| `output-b9-68d-x1000` | 0.987 [0.985,0.995] |
| `output-b9-69d-x1000` | 0.991 [0.976,1.021] |
| `output-b9-70d-x1000` | 0.985 [0.977,0.997] |
| `output-b9-128d-x500` | 0.990 [0.988,1.001] |
| `output-b9-1000d-x20` | 0.997 [0.890,1.000] |
| `output-b9-10000d-x1` | 0.999 [0.998,1.001] |
| `output-b10-1d-x50000` | 0.899 [0.895,0.901] |
| `output-b10-9d-x25000` | 0.537 [0.533,0.542] |
| `output-b10-10d-x25000` | 0.570 [0.548,0.572] |
| `output-b10-68d-x5000` | 0.467 [0.464,0.472] |
| `output-b10-69d-x5000` | 0.478 [0.474,0.481] |
| `output-b10-70d-x5000` | 0.481 [0.478,0.484] |
| `output-b10-128d-x2500` | 0.425 [0.423,0.433] |
| `output-b10-1000d-x2000` | 0.314 [0.313,0.316] |
| `output-b10-10000d-x100` | 0.281 [0.281,0.282] |
| `output-b11-1d-x10000` | 1.015 [0.993,1.051] |
| `output-b11-9d-x5000` | 0.984 [0.978,0.990] |
| `output-b11-10d-x5000` | 0.983 [0.965,0.986] |
| `output-b11-68d-x1000` | 0.985 [0.970,0.995] |
| `output-b11-69d-x1000` | 0.984 [0.973,0.995] |
| `output-b11-70d-x1000` | 0.995 [0.982,0.997] |
| `output-b11-128d-x500` | 0.999 [0.986,1.004] |
| `output-b11-1000d-x20` | 0.994 [0.876,1.003] |
| `output-b11-10000d-x1` | 0.998 [0.986,1.000] |
| `output-b12-1d-x10000` | 1.001 [0.990,1.024] |
| `output-b12-9d-x5000` | 0.980 [0.970,0.983] |
| `output-b12-10d-x5000` | 0.989 [0.980,0.994] |
| `output-b12-68d-x1000` | 0.990 [0.985,0.998] |
| `output-b12-69d-x1000` | 0.989 [0.981,1.010] |
| `output-b12-70d-x1000` | 0.993 [0.991,0.994] |
| `output-b12-128d-x500` | 0.995 [0.987,1.000] |
| `output-b12-1000d-x20` | 0.993 [0.885,1.003] |
| `output-b12-10000d-x1` | 1.000 [0.998,1.030] |
| `output-b13-1d-x10000` | 1.004 [0.997,1.035] |
| `output-b13-9d-x5000` | 0.986 [0.985,0.992] |
| `output-b13-10d-x5000` | 0.983 [0.978,0.991] |
| `output-b13-68d-x1000` | 0.975 [0.926,0.982] |
| `output-b13-69d-x1000` | 0.981 [0.966,0.990] |
| `output-b13-70d-x1000` | 0.984 [0.981,1.003] |
| `output-b13-128d-x500` | 0.996 [0.979,1.028] |
| `output-b13-1000d-x20` | 0.997 [0.880,0.999] |
| `output-b13-10000d-x1` | 0.999 [0.996,1.001] |
| `output-b14-1d-x10000` | 1.001 [0.996,1.066] |
| `output-b14-9d-x5000` | 0.989 [0.965,0.992] |
| `output-b14-10d-x5000` | 0.981 [0.962,0.998] |
| `output-b14-68d-x1000` | 0.985 [0.976,0.993] |
| `output-b14-69d-x1000` | 0.991 [0.986,0.995] |
| `output-b14-70d-x1000` | 0.986 [0.980,0.997] |
| `output-b14-128d-x500` | 0.988 [0.987,1.001] |
| `output-b14-1000d-x20` | 0.999 [0.876,1.032] |
| `output-b14-10000d-x1` | 1.000 [0.997,1.003] |
| `output-b15-1d-x10000` | 1.002 [0.993,1.004] |
| `output-b15-9d-x5000` | 0.982 [0.975,0.992] |
| `output-b15-10d-x5000` | 0.979 [0.971,0.989] |
| `output-b15-68d-x1000` | 0.987 [0.985,0.998] |
| `output-b15-69d-x1000` | 0.986 [0.966,0.996] |
| `output-b15-70d-x1000` | 0.986 [0.963,1.004] |
| `output-b15-128d-x500` | 0.987 [0.980,1.004] |
| `output-b15-1000d-x20` | 0.995 [0.883,1.005] |
| `output-b15-10000d-x1` | 1.000 [0.996,1.034] |
| `output-b16-1d-x10000` | 1.002 [0.992,1.059] |
| `output-b16-9d-x5000` | 0.986 [0.981,0.990] |
| `output-b16-10d-x5000` | 0.986 [0.979,0.994] |
| `output-b16-68d-x1000` | 0.986 [0.972,0.990] |
| `output-b16-69d-x1000` | 0.985 [0.974,1.008] |
| `output-b16-70d-x1000` | 0.983 [0.968,0.986] |
| `output-b16-128d-x500` | 0.987 [0.971,0.994] |
| `output-b16-1000d-x20` | 0.996 [0.899,1.001] |
| `output-b16-10000d-x1` | 0.999 [0.997,1.000] |
| `control-1d` | 0.995 [0.989,1.000] |
| `control-1000d` | 1.000 [0.979,1.013] |
| `control-10000d` | 0.972 [0.919,1.027] |

## Holdout intervals

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b2-3d-x10000` | 0.989 [0.977,0.991] |
| `output-b2-67d-x1000` | 0.982 [0.978,0.987] |
| `output-b2-71d-x1000` | 0.981 [0.972,0.988] |
| `output-b2-257d-x100` | 0.984 [0.933,0.993] |
| `output-b2-2048d-x5` | 1.000 [0.999,1.001] |
| `output-b8-3d-x10000` | 0.949 [0.940,0.959] |
| `output-b8-67d-x1000` | 0.986 [0.966,0.993] |
| `output-b8-71d-x1000` | 0.985 [0.976,0.990] |
| `output-b8-257d-x100` | 0.998 [0.988,1.020] |
| `output-b8-2048d-x5` | 0.999 [0.998,1.006] |
| `output-b10-3d-x50000` | 0.772 [0.751,0.772] |
| `output-b10-67d-x5000` | 0.469 [0.466,0.470] |
| `output-b10-71d-x5000` | 0.477 [0.476,0.493] |
| `output-b10-257d-x500` | 0.373 [0.372,0.376] |
| `output-b10-2048d-x500` | 0.297 [0.296,0.298] |
| `output-b16-3d-x10000` | 0.980 [0.978,0.985] |
| `output-b16-67d-x1000` | 0.989 [0.976,1.000] |
| `output-b16-71d-x1000` | 0.993 [0.979,0.997] |
| `output-b16-257d-x100` | 1.001 [0.994,1.004] |
| `output-b16-2048d-x5` | 1.000 [0.996,1.001] |
| `control-1d` | 1.000 [0.995,1.003] |
| `control-1000d` | 0.997 [0.988,1.009] |
| `control-10000d` | 1.000 [0.925,1.028] |


## Subsequent acceptance

This report retains the historical component source and decision. The distinct
[accepted native stage](accepted-native/README.md) records PR #334. Later
[combined conversion acceptance](../bc-206-combined-conversion/accepted-native/README.md)
completes #206 while preserving every earlier loss and inconclusive interval.
