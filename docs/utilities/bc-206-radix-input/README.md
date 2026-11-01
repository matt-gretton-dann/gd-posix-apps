# Bounded runtime-base input groups

Baseline `53e5c7d657239d2ca83b7d778f5fc181e86410da`; grouped source
`429e3bbcba8d7b067558208edefe05836daf5142`. The baseline contains only the
measurement protocol/oracles on accepted main, with the old input loop. It
does not include the held array implementation. Exact compiler commands,
library/header identity, flags and executable hashes are in the manifests.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, extensions ON, fixed
split point 126, profiling/coverage OFF. All 2760 serial invocations pass
exact result/checksum/status/stderr checks. One checked warm-up and nine
seeded paired rounds use 2000 bootstrap resamples; no local builds, tests
or profiles overlap these timings. Shared-host, frequency and thermal state
are uncontrolled. Component intervals include fixture validation, reset and
consumer work. Controls remain separate; they are never subtracted.

The 16-character threshold preserves the existing short-literal loop. The
new path uses fewer whole-coefficient MACs for long nondecimal text; it adds
no input-base assumption to compiled functions. Decimal input retains its
direct limb decoder. All three traits pass 36000 independent radix oracle
assertions before and after the change. Full Release: 178 cases / 628024
assertions and eight CLI checks. OFF ASan/UBSan: 137 arithmetic/VM cases /
443806 assertions plus 15 CLI checks. Eleven profiling checks also pass.

No measured interval is wholly above the provisional 1.05 regression
threshold. Small inputs and decimal results stay near parity; startup is
excluded from internal component intervals. These local results alone do
not establish portable optimality, a pure-kernel cost or native acceptance.
Fractional correctness is tested independently; these timing fixtures are
integers. Broader end-to-end and cross-platform observations remain required.

| Base / input digits | 1 | 8 | 15 | 16 | 17 | 32 | 128 | 1024 | 8192 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 2 | 0.996 [0.987,1.012] | 0.992 [0.966,1.015] | 0.990 [0.987,1.009] | 0.735 [0.729,0.737] | 0.736 [0.734,0.738] | 0.697 [0.685,0.702] | 0.455 [0.453,0.456] | 0.150 [0.149,0.154] | 0.058 [0.057,0.058] |
| 3 | 1.000 [0.998,1.010] | 0.997 [0.984,1.009] | 0.995 [0.985,0.997] | 0.738 [0.735,0.748] | 0.738 [0.735,0.741] | 0.641 [0.632,0.646] | 0.373 [0.371,0.375] | 0.151 [0.150,0.152] | 0.070 [0.070,0.072] |
| 4 | 1.000 [0.994,1.010] | 0.996 [0.995,1.002] | 0.996 [0.981,1.004] | 0.718 [0.714,0.729] | 0.746 [0.732,0.757] | 0.589 [0.584,0.594] | 0.332 [0.329,0.340] | 0.153 [0.151,0.157] | 0.083 [0.082,0.084] |
| 5 | 1.000 [0.990,1.003] | 0.995 [0.987,1.001] | 0.999 [0.955,1.013] | 0.677 [0.671,0.683] | 0.652 [0.644,0.660] | 0.577 [0.547,0.587] | 0.304 [0.302,0.307] | 0.156 [0.156,0.157] | 0.093 [0.092,0.093] |
| 6 | 1.000 [0.995,1.001] | 0.998 [0.995,1.004] | 0.990 [0.879,0.999] | 0.720 [0.710,0.728] | 0.697 [0.696,0.701] | 0.562 [0.546,0.562] | 0.318 [0.316,0.320] | 0.176 [0.176,0.178] | 0.103 [0.103,0.105] |
| 7 | 1.001 [0.982,1.009] | 1.006 [0.989,1.014] | 0.998 [0.991,1.008] | 0.684 [0.680,0.692] | 0.702 [0.692,0.714] | 0.633 [0.625,0.633] | 0.310 [0.308,0.311] | 0.183 [0.182,0.184] | 0.114 [0.113,0.116] |
| 8 | 0.997 [0.995,1.002] | 0.996 [0.985,1.006] | 0.999 [0.991,1.013] | 0.711 [0.703,0.714] | 0.722 [0.711,0.729] | 0.627 [0.621,0.659] | 0.309 [0.307,0.311] | 0.183 [0.181,0.186] | 0.115 [0.115,0.116] |
| 9 | 1.001 [0.995,1.004] | 0.997 [0.987,1.009] | 0.996 [0.990,1.012] | 0.707 [0.699,0.720] | 0.687 [0.682,0.694] | 0.644 [0.635,0.649] | 0.315 [0.312,0.318] | 0.190 [0.187,0.193] | 0.127 [0.126,0.128] |
| 10 | 0.993 [0.985,1.002] | 0.993 [0.967,0.996] | 0.991 [0.991,0.994] | 0.994 [0.974,0.996] | 0.994 [0.978,0.994] | 0.999 [0.992,1.017] | 0.998 [0.996,1.001] | 1.001 [0.999,1.013] | 0.994 [0.990,1.007] |
| 11 | 0.986 [0.981,0.992] | 0.996 [0.983,1.002] | 0.998 [0.983,1.010] | 0.725 [0.722,0.736] | 0.700 [0.693,0.716] | 0.645 [0.631,0.651] | 0.304 [0.302,0.307] | 0.185 [0.183,0.188] | 0.126 [0.125,0.126] |
| 12 | 0.989 [0.984,1.002] | 0.992 [0.982,0.998] | 0.986 [0.948,1.016] | 0.688 [0.683,0.696] | 0.687 [0.684,0.693] | 0.567 [0.540,0.569] | 0.298 [0.295,0.302] | 0.193 [0.190,0.199] | 0.138 [0.137,0.138] |
| 13 | 0.985 [0.983,1.008] | 0.992 [0.985,1.020] | 0.991 [0.984,1.002] | 0.675 [0.667,0.680] | 0.684 [0.679,0.694] | 0.630 [0.621,0.632] | 0.297 [0.294,0.304] | 0.188 [0.186,0.191] | 0.137 [0.135,0.137] |
| 14 | 0.984 [0.981,0.985] | 0.989 [0.980,0.997] | 0.988 [0.983,0.997] | 0.700 [0.698,0.703] | 0.738 [0.731,0.744] | 0.605 [0.601,0.613] | 0.288 [0.287,0.293] | 0.187 [0.184,0.188] | 0.135 [0.135,0.136] |
| 15 | 0.985 [0.980,1.000] | 0.999 [0.996,1.015] | 1.006 [0.986,1.012] | 0.702 [0.698,0.715] | 0.730 [0.724,0.739] | 0.617 [0.610,0.628] | 0.299 [0.295,0.300] | 0.184 [0.183,0.186] | 0.135 [0.134,0.135] |
| 16 | 0.985 [0.971,0.988] | 0.996 [0.991,1.009] | 0.991 [0.984,1.002] | 0.733 [0.732,0.744] | 0.726 [0.720,0.732] | 0.617 [0.601,0.659] | 0.310 [0.306,0.317] | 0.199 [0.197,0.201] | 0.152 [0.152,0.155] |

Complete raw samples, controls, fixtures and intervals are retained. Batched
output, other output-base algorithms and final #206 acceptance remain open.
