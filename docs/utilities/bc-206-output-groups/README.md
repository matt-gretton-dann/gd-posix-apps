# Bounded nondecimal output groups

Exact baseline `7424aacc39b79b3d46c534be6ec4595643c2b7f0`; candidate
`ca006614732c55a49d18bccc91495e9858afa682`. Both are frozen before later
array integration. No later source is substituted into these observations.
Actual compiler/library identity, flags, commands and binary hashes remain in
the release/component recipes.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, extensions ON, fixed
split point 126, profiling/coverage OFF. All 4860 full and 460 held-out
component invocations pass independent integer-radix and continuation-length
oracles, exact output/checksum/status/stderr and deadline checks. Full and
held-out use different seeds/sizes, one checked warm-up and nine shuffled
paired rounds with 2000 bootstrap resamples. No local builds/tests/profiles
overlap timings. Shared-host scheduling, frequency and thermals are uncontrolled.

Internal intervals include conversion, formatting/reset and consumer work;
startup/fixture parsing are excluded. Controls stay separate and are never
subtracted. Positive integer fixtures do not cover every fraction/error/state
regime. Independent all-trait tests cover those values and positions separately.

## Algorithm and cutoff

For bases 2 through 16 and at least four storage limbs, divide by the largest
output-radix power strictly below the storage radix. Expand each native
remainder, padding lower groups to exact width. The most significant group is
unpadded. The buffer is bounded by native bit count and exposes initialized
bytes only. Short values, one-digit group widths and space-separated bases
above 16 retain the scalar path. Fractional conversion stays unchanged.

For native 10^9 limbs, four limbs start at 28 decimal digits. The retained
26/27/28/29 and 35/36/37 fixtures measure that decision directly. Below the
cutoff, nondecimal medians stay near parity; at 28 digits they span about
0.36–0.59. At 10000 digits they span about 0.04–0.15. No full/held-out
interval is wholly above the provisional 1.05 regression threshold. Decimal
output stays on its prior path and remains visible in the tables. This is a
measured local cutoff, not a universal optimum or native acceptance claim.

The character-vector decimal-division oracle passes 17661 assertions before
the change and after it; base-20/99 power fixtures add six fallback assertions.
After accepted-tuner integration: 188 Release cases / 645726 assertions plus
eight CLI/tool checks. OFF ASan/UBSan: 146 arithmetic/VM cases / 461494
assertions and 15 CLI checks. Six profiling assertions across three traits
confirm fewer whole-coefficient divisions, or the narrow-trait scalar fallback;
eleven profiling CLI checks pass. Counters are diagnostics, not elapsed timings.

The native benchmark matrix previously lacked nondecimal output. #216 adds
separate binary/octal output classes before this runtime candidate can be
accepted. Every new native result must name its actual integrated source.
Divide-and-conquer input/output evaluation, decimal formatting-buffer reuse
and final #206 acceptance remain separate work.

## Full intervals

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b2-1d-x10000` | 0.991 [0.988,0.993] |
| `output-b2-9d-x5000` | 1.004 [0.994,1.009] |
| `output-b2-10d-x5000` | 1.007 [1.005,1.009] |
| `output-b2-26d-x2000` | 1.007 [1.005,1.011] |
| `output-b2-27d-x2000` | 1.014 [1.010,1.021] |
| `output-b2-28d-x2000` | 0.361 [0.358,0.365] |
| `output-b2-29d-x2000` | 0.378 [0.376,0.385] |
| `output-b2-35d-x2000` | 0.335 [0.332,0.338] |
| `output-b2-36d-x2000` | 0.334 [0.331,0.347] |
| `output-b2-37d-x2000` | 0.329 [0.327,0.333] |
| `output-b2-68d-x1000` | 0.242 [0.242,0.250] |
| `output-b2-69d-x1000` | 0.238 [0.236,0.247] |
| `output-b2-70d-x1000` | 0.238 [0.234,0.242] |
| `output-b2-128d-x500` | 0.162 [0.161,0.169] |
| `output-b2-1000d-x20` | 0.050 [0.050,0.050] |
| `output-b2-10000d-x1` | 0.036 [0.036,0.036] |
| `output-b3-1d-x10000` | 0.987 [0.980,0.993] |
| `output-b3-9d-x5000` | 1.018 [0.996,1.023] |
| `output-b3-10d-x5000` | 0.998 [0.991,1.011] |
| `output-b3-26d-x2000` | 1.005 [0.995,1.008] |
| `output-b3-27d-x2000` | 1.004 [0.998,1.008] |
| `output-b3-28d-x2000` | 0.416 [0.411,0.417] |
| `output-b3-29d-x2000` | 0.400 [0.397,0.403] |
| `output-b3-35d-x2000` | 0.369 [0.365,0.370] |
| `output-b3-36d-x2000` | 0.365 [0.360,0.376] |
| `output-b3-37d-x2000` | 0.363 [0.361,0.367] |
| `output-b3-68d-x1000` | 0.269 [0.268,0.274] |
| `output-b3-69d-x1000` | 0.269 [0.268,0.274] |
| `output-b3-70d-x1000` | 0.268 [0.266,0.272] |
| `output-b3-128d-x500` | 0.194 [0.190,0.195] |
| `output-b3-1000d-x20` | 0.074 [0.074,0.074] |
| `output-b3-10000d-x1` | 0.060 [0.060,0.060] |
| `output-b4-1d-x10000` | 0.994 [0.988,0.998] |
| `output-b4-9d-x5000` | 0.997 [0.996,1.034] |
| `output-b4-10d-x5000` | 1.010 [0.991,1.032] |
| `output-b4-26d-x2000` | 1.006 [1.001,1.017] |
| `output-b4-27d-x2000` | 1.000 [0.998,1.012] |
| `output-b4-28d-x2000` | 0.421 [0.421,0.424] |
| `output-b4-29d-x2000` | 0.434 [0.433,0.441] |
| `output-b4-35d-x2000` | 0.402 [0.398,0.405] |
| `output-b4-36d-x2000` | 0.391 [0.390,0.395] |
| `output-b4-37d-x2000` | 0.392 [0.387,0.394] |
| `output-b4-68d-x1000` | 0.313 [0.305,0.319] |
| `output-b4-69d-x1000` | 0.307 [0.306,0.312] |
| `output-b4-70d-x1000` | 0.301 [0.284,0.306] |
| `output-b4-128d-x500` | 0.221 [0.220,0.223] |
| `output-b4-1000d-x20` | 0.089 [0.089,0.090] |
| `output-b4-10000d-x1` | 0.073 [0.073,0.073] |
| `output-b5-1d-x10000` | 0.991 [0.985,0.992] |
| `output-b5-9d-x5000` | 0.997 [0.988,1.000] |
| `output-b5-10d-x5000` | 0.995 [0.957,1.016] |
| `output-b5-26d-x2000` | 1.008 [1.002,1.015] |
| `output-b5-27d-x2000` | 1.006 [0.998,1.008] |
| `output-b5-28d-x2000` | 0.435 [0.433,0.437] |
| `output-b5-29d-x2000` | 0.430 [0.428,0.437] |
| `output-b5-35d-x2000` | 0.429 [0.424,0.430] |
| `output-b5-36d-x2000` | 0.428 [0.422,0.435] |
| `output-b5-37d-x2000` | 0.426 [0.425,0.431] |
| `output-b5-68d-x1000` | 0.330 [0.325,0.334] |
| `output-b5-69d-x1000` | 0.333 [0.328,0.338] |
| `output-b5-70d-x1000` | 0.328 [0.326,0.330] |
| `output-b5-128d-x500` | 0.231 [0.230,0.234] |
| `output-b5-1000d-x20` | 0.104 [0.104,0.106] |
| `output-b5-10000d-x1` | 0.089 [0.089,0.089] |
| `output-b6-1d-x10000` | 0.990 [0.986,0.992] |
| `output-b6-9d-x5000` | 0.996 [0.992,1.008] |
| `output-b6-10d-x5000` | 0.994 [0.968,1.014] |
| `output-b6-26d-x2000` | 1.007 [1.005,1.015] |
| `output-b6-27d-x2000` | 1.007 [1.003,1.036] |
| `output-b6-28d-x2000` | 0.455 [0.442,0.459] |
| `output-b6-29d-x2000` | 0.447 [0.445,0.449] |
| `output-b6-35d-x2000` | 0.413 [0.411,0.415] |
| `output-b6-36d-x2000` | 0.403 [0.400,0.412] |
| `output-b6-37d-x2000` | 0.443 [0.440,0.446] |
| `output-b6-68d-x1000` | 0.327 [0.325,0.334] |
| `output-b6-69d-x1000` | 0.330 [0.328,0.343] |
| `output-b6-70d-x1000` | 0.340 [0.339,0.343] |
| `output-b6-128d-x500` | 0.242 [0.236,0.244] |
| `output-b6-1000d-x20` | 0.112 [0.111,0.113] |
| `output-b6-10000d-x1` | 0.096 [0.096,0.096] |
| `output-b7-1d-x10000` | 0.991 [0.986,0.996] |
| `output-b7-9d-x5000` | 1.005 [0.998,1.017] |
| `output-b7-10d-x5000` | 0.997 [0.994,1.002] |
| `output-b7-26d-x2000` | 1.030 [1.023,1.048] |
| `output-b7-27d-x2000` | 1.003 [0.988,1.017] |
| `output-b7-28d-x2000` | 0.468 [0.467,0.470] |
| `output-b7-29d-x2000` | 0.457 [0.450,0.462] |
| `output-b7-35d-x2000` | 0.421 [0.414,0.422] |
| `output-b7-36d-x2000` | 0.421 [0.416,0.422] |
| `output-b7-37d-x2000` | 0.421 [0.419,0.424] |
| `output-b7-68d-x1000` | 0.343 [0.340,0.345] |
| `output-b7-69d-x1000` | 0.349 [0.348,0.352] |
| `output-b7-70d-x1000` | 0.350 [0.347,0.356] |
| `output-b7-128d-x500` | 0.259 [0.252,0.264] |
| `output-b7-1000d-x20` | 0.121 [0.121,0.122] |
| `output-b7-10000d-x1` | 0.105 [0.105,0.105] |
| `output-b8-1d-x10000` | 0.988 [0.975,0.991] |
| `output-b8-9d-x5000` | 0.999 [0.986,1.006] |
| `output-b8-10d-x5000` | 0.997 [0.995,0.999] |
| `output-b8-26d-x2000` | 1.004 [0.997,1.013] |
| `output-b8-27d-x2000` | 1.027 [1.017,1.030] |
| `output-b8-28d-x2000` | 0.502 [0.498,0.504] |
| `output-b8-29d-x2000` | 0.475 [0.467,0.479] |
| `output-b8-35d-x2000` | 0.439 [0.437,0.443] |
| `output-b8-36d-x2000` | 0.434 [0.432,0.441] |
| `output-b8-37d-x2000` | 0.436 [0.429,0.441] |
| `output-b8-68d-x1000` | 0.364 [0.364,0.380] |
| `output-b8-69d-x1000` | 0.365 [0.361,0.368] |
| `output-b8-70d-x1000` | 0.358 [0.354,0.360] |
| `output-b8-128d-x500` | 0.277 [0.274,0.279] |
| `output-b8-1000d-x20` | 0.130 [0.130,0.131] |
| `output-b8-10000d-x1` | 0.113 [0.110,0.113] |
| `output-b9-1d-x10000` | 0.989 [0.983,0.993] |
| `output-b9-9d-x5000` | 0.996 [0.994,1.005] |
| `output-b9-10d-x5000` | 0.997 [0.989,1.005] |
| `output-b9-26d-x2000` | 1.009 [1.004,1.022] |
| `output-b9-27d-x2000` | 1.001 [0.997,1.012] |
| `output-b9-28d-x2000` | 0.501 [0.496,0.509] |
| `output-b9-29d-x2000` | 0.489 [0.487,0.497] |
| `output-b9-35d-x2000` | 0.450 [0.444,0.451] |
| `output-b9-36d-x2000` | 0.449 [0.448,0.450] |
| `output-b9-37d-x2000` | 0.441 [0.438,0.458] |
| `output-b9-68d-x1000` | 0.373 [0.367,0.381] |
| `output-b9-69d-x1000` | 0.363 [0.359,0.374] |
| `output-b9-70d-x1000` | 0.364 [0.363,0.369] |
| `output-b9-128d-x500` | 0.276 [0.273,0.303] |
| `output-b9-1000d-x20` | 0.132 [0.131,0.133] |
| `output-b9-10000d-x1` | 0.115 [0.115,0.116] |
| `output-b10-1d-x50000` | 0.985 [0.982,0.989] |
| `output-b10-9d-x25000` | 0.987 [0.985,0.999] |
| `output-b10-10d-x25000` | 0.989 [0.976,0.992] |
| `output-b10-26d-x10000` | 1.003 [0.996,1.006] |
| `output-b10-27d-x10000` | 0.999 [0.979,1.007] |
| `output-b10-28d-x10000` | 1.007 [0.999,1.012] |
| `output-b10-29d-x10000` | 1.006 [0.995,1.008] |
| `output-b10-35d-x10000` | 1.002 [0.997,1.011] |
| `output-b10-36d-x10000` | 1.008 [0.997,1.024] |
| `output-b10-37d-x10000` | 1.006 [1.000,1.025] |
| `output-b10-68d-x5000` | 1.004 [0.994,1.015] |
| `output-b10-69d-x5000` | 0.997 [0.991,1.005] |
| `output-b10-70d-x5000` | 0.997 [0.977,1.014] |
| `output-b10-128d-x2500` | 1.000 [0.998,1.006] |
| `output-b10-1000d-x2000` | 1.001 [0.990,1.014] |
| `output-b10-10000d-x100` | 1.002 [1.000,1.006] |
| `output-b11-1d-x10000` | 0.988 [0.965,0.989] |
| `output-b11-9d-x5000` | 1.005 [1.001,1.009] |
| `output-b11-10d-x5000` | 1.000 [0.997,1.006] |
| `output-b11-26d-x2000` | 1.006 [1.001,1.017] |
| `output-b11-27d-x2000` | 1.003 [0.991,1.010] |
| `output-b11-28d-x2000` | 0.528 [0.523,0.537] |
| `output-b11-29d-x2000` | 0.515 [0.512,0.518] |
| `output-b11-35d-x2000` | 0.471 [0.468,0.472] |
| `output-b11-36d-x2000` | 0.465 [0.464,0.475] |
| `output-b11-37d-x2000` | 0.467 [0.459,0.467] |
| `output-b11-68d-x1000` | 0.370 [0.364,0.372] |
| `output-b11-69d-x1000` | 0.376 [0.375,0.380] |
| `output-b11-70d-x1000` | 0.383 [0.378,0.389] |
| `output-b11-128d-x500` | 0.296 [0.289,0.299] |
| `output-b11-1000d-x20` | 0.147 [0.146,0.148] |
| `output-b11-10000d-x1` | 0.129 [0.129,0.130] |
| `output-b12-1d-x10000` | 0.987 [0.969,0.993] |
| `output-b12-9d-x5000` | 1.004 [0.996,1.012] |
| `output-b12-10d-x5000` | 0.999 [0.994,1.006] |
| `output-b12-26d-x2000` | 0.998 [0.994,1.005] |
| `output-b12-27d-x2000` | 1.002 [0.996,1.008] |
| `output-b12-28d-x2000` | 0.543 [0.532,0.549] |
| `output-b12-29d-x2000` | 0.523 [0.515,0.529] |
| `output-b12-35d-x2000` | 0.474 [0.470,0.482] |
| `output-b12-36d-x2000` | 0.472 [0.470,0.476] |
| `output-b12-37d-x2000` | 0.470 [0.467,0.475] |
| `output-b12-68d-x1000` | 0.392 [0.388,0.408] |
| `output-b12-69d-x1000` | 0.377 [0.369,0.382] |
| `output-b12-70d-x1000` | 0.386 [0.380,0.389] |
| `output-b12-128d-x500` | 0.297 [0.297,0.303] |
| `output-b12-1000d-x20` | 0.146 [0.145,0.148] |
| `output-b12-10000d-x1` | 0.129 [0.128,0.129] |
| `output-b13-1d-x10000` | 0.991 [0.986,0.992] |
| `output-b13-9d-x5000` | 1.001 [0.987,1.008] |
| `output-b13-10d-x5000` | 1.000 [0.996,1.007] |
| `output-b13-26d-x2000` | 0.992 [0.980,1.009] |
| `output-b13-27d-x2000` | 1.007 [1.004,1.010] |
| `output-b13-28d-x2000` | 0.536 [0.530,0.546] |
| `output-b13-29d-x2000` | 0.536 [0.533,0.543] |
| `output-b13-35d-x2000` | 0.468 [0.466,0.476] |
| `output-b13-36d-x2000` | 0.472 [0.470,0.478] |
| `output-b13-37d-x2000` | 0.481 [0.479,0.483] |
| `output-b13-68d-x1000` | 0.404 [0.402,0.406] |
| `output-b13-69d-x1000` | 0.390 [0.386,0.396] |
| `output-b13-70d-x1000` | 0.380 [0.378,0.382] |
| `output-b13-128d-x500` | 0.307 [0.304,0.313] |
| `output-b13-1000d-x20` | 0.148 [0.147,0.149] |
| `output-b13-10000d-x1` | 0.128 [0.127,0.129] |
| `output-b14-1d-x10000` | 0.991 [0.980,0.993] |
| `output-b14-9d-x5000` | 1.001 [0.995,1.011] |
| `output-b14-10d-x5000` | 0.999 [0.995,1.000] |
| `output-b14-26d-x2000` | 0.998 [0.995,1.004] |
| `output-b14-27d-x2000` | 1.000 [0.997,1.005] |
| `output-b14-28d-x2000` | 0.548 [0.544,0.556] |
| `output-b14-29d-x2000` | 0.544 [0.537,0.552] |
| `output-b14-35d-x2000` | 0.516 [0.509,0.520] |
| `output-b14-36d-x2000` | 0.511 [0.506,0.514] |
| `output-b14-37d-x2000` | 0.501 [0.496,0.508] |
| `output-b14-68d-x1000` | 0.441 [0.437,0.442] |
| `output-b14-69d-x1000` | 0.435 [0.431,0.440] |
| `output-b14-70d-x1000` | 0.414 [0.407,0.420] |
| `output-b14-128d-x500` | 0.332 [0.327,0.338] |
| `output-b14-1000d-x20` | 0.167 [0.166,0.168] |
| `output-b14-10000d-x1` | 0.147 [0.146,0.147] |
| `output-b15-1d-x10000` | 0.984 [0.971,0.990] |
| `output-b15-9d-x5000` | 1.000 [0.995,1.012] |
| `output-b15-10d-x5000` | 1.001 [0.998,1.010] |
| `output-b15-26d-x2000` | 0.993 [0.986,1.000] |
| `output-b15-27d-x2000` | 0.998 [0.983,1.003] |
| `output-b15-28d-x2000` | 0.585 [0.578,0.587] |
| `output-b15-29d-x2000` | 0.545 [0.542,0.553] |
| `output-b15-35d-x2000` | 0.509 [0.504,0.514] |
| `output-b15-36d-x2000` | 0.517 [0.511,0.521] |
| `output-b15-37d-x2000` | 0.515 [0.504,0.518] |
| `output-b15-68d-x1000` | 0.437 [0.432,0.446] |
| `output-b15-69d-x1000` | 0.443 [0.441,0.458] |
| `output-b15-70d-x1000` | 0.420 [0.414,0.426] |
| `output-b15-128d-x500` | 0.336 [0.329,0.342] |
| `output-b15-1000d-x20` | 0.167 [0.166,0.168] |
| `output-b15-10000d-x1` | 0.146 [0.146,0.147] |
| `output-b16-1d-x10000` | 0.987 [0.941,0.993] |
| `output-b16-9d-x5000` | 0.996 [0.994,1.006] |
| `output-b16-10d-x5000` | 1.000 [0.995,1.015] |
| `output-b16-26d-x2000` | 0.994 [0.991,0.999] |
| `output-b16-27d-x2000` | 1.000 [0.995,1.026] |
| `output-b16-28d-x2000` | 0.571 [0.566,0.573] |
| `output-b16-29d-x2000` | 0.562 [0.559,0.576] |
| `output-b16-35d-x2000` | 0.526 [0.522,0.530] |
| `output-b16-36d-x2000` | 0.521 [0.515,0.524] |
| `output-b16-37d-x2000` | 0.523 [0.518,0.529] |
| `output-b16-68d-x1000` | 0.441 [0.419,0.445] |
| `output-b16-69d-x1000` | 0.436 [0.434,0.447] |
| `output-b16-70d-x1000` | 0.435 [0.429,0.444] |
| `output-b16-128d-x500` | 0.340 [0.338,0.344] |
| `output-b16-1000d-x20` | 0.167 [0.166,0.169] |
| `output-b16-10000d-x1` | 0.145 [0.145,0.145] |
| `control-1d` | 1.000 [0.995,1.008] |
| `control-1000d` | 1.000 [0.997,1.009] |
| `control-10000d` | 0.973 [0.949,1.001] |

## Holdout intervals

| Workload | Candidate / baseline, 95% paired interval |
| --- | --- |
| `output-b2-3d-x10000` | 1.002 [0.997,1.009] |
| `output-b2-67d-x1000` | 0.240 [0.238,0.246] |
| `output-b2-71d-x1000` | 0.242 [0.241,0.246] |
| `output-b2-257d-x100` | 0.116 [0.115,0.120] |
| `output-b2-2048d-x5` | 0.041 [0.041,0.042] |
| `output-b8-3d-x10000` | 0.987 [0.982,0.991] |
| `output-b8-67d-x1000` | 0.366 [0.365,0.370] |
| `output-b8-71d-x1000` | 0.355 [0.352,0.358] |
| `output-b8-257d-x100` | 0.218 [0.216,0.220] |
| `output-b8-2048d-x5` | 0.119 [0.118,0.119] |
| `output-b10-3d-x50000` | 0.989 [0.987,0.993] |
| `output-b10-67d-x5000` | 1.002 [0.990,1.010] |
| `output-b10-71d-x5000` | 0.998 [0.990,1.011] |
| `output-b10-257d-x500` | 0.998 [0.994,1.009] |
| `output-b10-2048d-x500` | 1.003 [1.001,1.006] |
| `output-b16-3d-x10000` | 0.994 [0.991,0.999] |
| `output-b16-67d-x1000` | 0.431 [0.429,0.433] |
| `output-b16-71d-x1000` | 0.427 [0.426,0.434] |
| `output-b16-257d-x100` | 0.276 [0.270,0.278] |
| `output-b16-2048d-x5` | 0.153 [0.152,0.153] |
| `control-1d` | 0.998 [0.995,1.007] |
| `control-1000d` | 1.006 [0.994,1.009] |
| `control-10000d` | 1.086 [1.028,1.148] |

