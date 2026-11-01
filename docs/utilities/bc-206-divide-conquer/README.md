# Integer divide-and-conquer conversion experiments

Exact source `376235c8b530bdeea1d91d26d474c85a522e5c57`; both ordinary grouped
and diagnostic divide-and-conquer modes run in the same frozen executable.
All actual flags, compiler/library identity, commands and executable hashes
remain in the recipe/manifest files. No later runtime integration is substituted
into these measurements. This stage changes benchmark diagnostics only.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, extensions ON, fixed
split point 126, profiling/coverage OFF. Full 2200, held-out 600 and independent
cutoff-sweep 480 invocations pass exact Python integer/radix and wrapped-length
checks, stdout checksum, status, stderr and deadlines. Each uses one checked
warm-up and nine shuffled paired rounds, 2000 bootstrap resamples. Full, held-out
and cutoff sweep use distinct seeds/lengths. No local builds/tests/profiles
overlap timing; shared-host scheduling, frequency and thermals remain uncontrolled.

Every DC conversion owns its local power cache and includes generation,
allocation and cleanup in the interval. Input splits exact integer coefficients;
output uses exact quotient/remainder and padding. Its integer log estimate
chooses only the partition. Public-API leaf output formatting/continuation joining
is included. A fixed 64-character/decimal-digit leaf is a prototype choice,
not an optimum or a lower bound for all divide-and-conquer implementations.
Signed/fractional diagnostic fixtures are rejected; production contracts are
unchanged. Decimal paths retain their linear decoder/formatter.

## Decisions supported by these experiments

Input DC loses at modest sizes (often roughly 1.7–2.6x near 65–256 characters).
At 100000 nondecimal input characters its ratio is about 0.13–0.26. Held-out
120000-character binary/hex input ratios are about 0.237/0.123. A separate
32767/32768/32769 and 65535/65536/65537 sweep retains odd-partition costs:
binary input near 65536 is 0.321, while 65537 is 0.347. These are not monotonic
curves. A conservative candidate cutoff of 65536 source characters has margin
for every measured nondecimal base; it still needs actual runtime/native
evaluation. Short and fractional forms should retain generic grouped decoding.
This is a proposed portable policy, not an achieved universal crossover.

Output DC loses in every measured nondecimal regime above the leaf, including
about 1.18–1.36x at 100000 decimal digits and 1.29–1.34x for held-out 120000
binary/hex output. Retain grouped runtime output. This rejects this public-API
prototype over these regimes, not every possible future DC/fast-division design.
Every losing and inconclusive interval remains in the tables and raw data.

Ten component groups pass Release, extensions-OFF ASan/UBSan and profiling
builds. They cover all bases 2–16, zero, leading zeros, 64/65/127/128/1024
boundaries, exact radix powers/zero remainders and diagnostic domain rejection.
The original missing-mode oracle failures are retained before implementation.

## Separate controls and process memory

Thirty separate copy/reset/length-consumer controls pass exact checks. They
inherit the collector environment and do not establish a pure-kernel cost;
compiler folding can especially affect small control loops. They are never
subtracted from conversion intervals. Their raw internal durations remain.

Twenty-four fresh-process peak-memory diagnostics (three per case/mode) pass
the same exact oracles. The local getrusage(2) reference confirms raw RSS bytes.
These process peaks include loader, the one-MiB fixture buffer, preparsed Number,
conversion temporaries, formatting and final JSON. They are neither isolated
algorithm workspace nor selected allocator payloads. Frequency/scheduling are
uncontrolled; diagnostic user/system times are not used as speed evidence.

| Regime, 100000 digits | Algorithm | Median peak MiB | Min | Max |
| --- | --- | ---: | ---: | ---: |
| `in-b16-100000d-x1` | grouped | 3.969 | 3.953 | 3.969 |
| `in-b16-100000d-x1` | divide_conquer | 4.516 | 4.484 | 4.531 |
| `in-b2-100000d-x1` | grouped | 3.359 | 3.359 | 3.391 |
| `in-b2-100000d-x1` | divide_conquer | 3.641 | 3.641 | 3.641 |
| `out-b16-100000d-x1` | grouped | 3.516 | 3.516 | 3.516 |
| `out-b16-100000d-x1` | divide_conquer | 4.672 | 4.656 | 4.672 |
| `out-b2-100000d-x1` | grouped | 4.594 | 4.594 | 4.594 |
| `out-b2-100000d-x1` | divide_conquer | 5.969 | 5.953 | 6.109 |

The faster input prototype uses more process memory in these samples: roughly
0.28 MiB for binary and 0.55 MiB for hexadecimal at 100000 input characters.
The slower output prototype also has larger observed peaks. Three observations
do not establish a portable peak or confidence interval; full raw samples remain.

## full intervals

| Workload | DC / grouped, 95% paired interval |
| --- | --- |
| `in-b2-1d-x100000` | 1.018 [1.015,1.021] |
| `out-b2-1d-x100000` | 1.014 [1.013,1.016] |
| `in-b2-16d-x10000` | 1.011 [0.998,1.043] |
| `out-b2-16d-x10000` | 1.003 [0.977,1.009] |
| `in-b2-63d-x2000` | 1.001 [0.997,1.003] |
| `out-b2-63d-x2000` | 0.999 [0.962,1.013] |
| `in-b2-64d-x2000` | 1.001 [0.997,1.005] |
| `out-b2-64d-x2000` | 1.015 [0.967,1.027] |
| `in-b2-65d-x2000` | 2.432 [2.416,2.440] |
| `out-b2-65d-x2000` | 2.641 [2.605,2.664] |
| `in-b2-127d-x500` | 1.755 [1.749,1.756] |
| `out-b2-127d-x500` | 2.108 [2.059,2.155] |
| `in-b2-128d-x500` | 1.745 [1.740,1.748] |
| `out-b2-128d-x500` | 2.894 [2.733,2.945] |
| `in-b2-256d-x500` | 1.957 [1.944,1.966] |
| `out-b2-256d-x500` | 2.924 [2.905,2.940] |
| `in-b2-1024d-x20` | 1.861 [1.813,1.862] |
| `out-b2-1024d-x20` | 2.206 [2.147,2.241] |
| `in-b2-10000d-x2` | 0.960 [0.956,0.967] |
| `out-b2-10000d-x2` | 1.566 [1.559,1.571] |
| `in-b2-100000d-x1` | 0.259 [0.258,0.262] |
| `out-b2-100000d-x1` | 1.356 [1.353,1.358] |
| `in-b3-1d-x100000` | 1.017 [1.013,1.018] |
| `out-b3-1d-x100000` | 1.017 [1.014,1.023] |
| `in-b3-16d-x10000` | 1.004 [1.000,1.006] |
| `out-b3-16d-x10000` | 1.005 [1.002,1.014] |
| `in-b3-63d-x2000` | 1.003 [0.989,1.006] |
| `out-b3-63d-x2000` | 1.002 [0.997,1.015] |
| `in-b3-64d-x2000` | 1.002 [0.997,1.004] |
| `out-b3-64d-x2000` | 1.003 [0.943,1.015] |
| `in-b3-65d-x2000` | 2.114 [2.099,2.124] |
| `out-b3-65d-x2000` | 2.792 [2.761,2.812] |
| `in-b3-127d-x500` | 1.818 [1.773,1.831] |
| `out-b3-127d-x500` | 2.274 [2.219,2.288] |
| `in-b3-128d-x500` | 1.794 [1.762,1.811] |
| `out-b3-128d-x500` | 2.245 [2.226,2.321] |
| `in-b3-256d-x500` | 2.006 [1.997,2.035] |
| `out-b3-256d-x500` | 2.965 [2.961,2.985] |
| `in-b3-1024d-x20` | 1.745 [1.707,1.765] |
| `out-b3-1024d-x20` | 1.892 [1.868,1.905] |
| `in-b3-10000d-x2` | 0.682 [0.678,0.693] |
| `out-b3-10000d-x2` | 1.379 [1.373,1.383] |
| `in-b3-100000d-x1` | 0.203 [0.202,0.203] |
| `out-b3-100000d-x1` | 1.179 [1.173,1.180] |
| `in-b8-1d-x100000` | 1.018 [1.017,1.021] |
| `out-b8-1d-x100000` | 1.018 [1.013,1.022] |
| `in-b8-16d-x10000` | 1.003 [0.999,1.005] |
| `out-b8-16d-x10000` | 1.003 [0.989,1.012] |
| `in-b8-63d-x2000` | 1.005 [0.996,1.007] |
| `out-b8-63d-x2000` | 1.007 [0.996,1.024] |
| `in-b8-64d-x2000` | 1.006 [0.993,1.009] |
| `out-b8-64d-x2000` | 1.008 [1.000,1.019] |
| `in-b8-65d-x2000` | 2.550 [2.525,2.566] |
| `out-b8-65d-x2000` | 3.005 [2.992,3.027] |
| `in-b8-127d-x500` | 1.938 [1.921,1.952] |
| `out-b8-127d-x500` | 2.488 [2.465,2.507] |
| `in-b8-128d-x500` | 1.921 [1.903,1.954] |
| `out-b8-128d-x500` | 3.568 [3.534,3.581] |
| `in-b8-256d-x500` | 1.995 [1.975,2.014] |
| `out-b8-256d-x500` | 3.753 [3.722,3.772] |
| `in-b8-1024d-x20` | 1.269 [1.235,1.283] |
| `out-b8-1024d-x20` | 2.378 [2.321,2.394] |
| `in-b8-10000d-x2` | 0.449 [0.448,0.452] |
| `out-b8-10000d-x2` | 1.460 [1.455,1.465] |
| `in-b8-100000d-x1` | 0.150 [0.148,0.150] |
| `out-b8-100000d-x1` | 1.255 [1.245,1.262] |
| `in-b10-1d-x100000` | 1.021 [1.019,1.022] |
| `out-b10-1d-x100000` | 1.003 [0.991,1.013] |
| `in-b10-16d-x10000` | 1.011 [1.005,1.012] |
| `out-b10-16d-x10000` | 1.003 [0.989,1.005] |
| `in-b10-63d-x2000` | 1.000 [0.988,1.018] |
| `out-b10-63d-x2000` | 1.003 [0.996,1.011] |
| `in-b10-64d-x2000` | 1.014 [0.973,1.038] |
| `out-b10-64d-x2000` | 1.002 [0.999,1.005] |
| `in-b10-65d-x2000` | 1.012 [0.982,1.019] |
| `out-b10-65d-x2000` | 0.996 [0.990,1.008] |
| `in-b10-127d-x500` | 1.000 [0.987,1.006] |
| `out-b10-127d-x500` | 1.005 [0.992,1.018] |
| `in-b10-128d-x500` | 0.995 [0.978,1.000] |
| `out-b10-128d-x500` | 0.999 [0.997,1.005] |
| `in-b10-256d-x500` | 1.000 [0.994,1.002] |
| `out-b10-256d-x500` | 0.999 [0.993,1.013] |
| `in-b10-1024d-x20` | 1.022 [0.997,1.041] |
| `out-b10-1024d-x20` | 1.012 [1.007,1.025] |
| `in-b10-10000d-x2` | 0.991 [0.936,1.001] |
| `out-b10-10000d-x2` | 1.003 [0.981,1.029] |
| `in-b10-100000d-x1` | 1.004 [0.995,1.013] |
| `out-b10-100000d-x1` | 1.003 [0.999,1.016] |
| `in-b16-1d-x100000` | 1.015 [1.012,1.017] |
| `out-b16-1d-x100000` | 1.018 [1.015,1.021] |
| `in-b16-16d-x10000` | 1.004 [0.998,1.007] |
| `out-b16-16d-x10000` | 1.013 [1.010,1.014] |
| `in-b16-63d-x2000` | 1.004 [0.995,1.111] |
| `out-b16-63d-x2000` | 1.009 [1.003,1.016] |
| `in-b16-64d-x2000` | 0.998 [0.991,1.002] |
| `out-b16-64d-x2000` | 1.008 [0.998,1.016] |
| `in-b16-65d-x2000` | 2.438 [2.397,2.468] |
| `out-b16-65d-x2000` | 2.989 [2.951,3.034] |
| `in-b16-127d-x500` | 1.822 [1.786,1.853] |
| `out-b16-127d-x500` | 3.510 [3.461,3.562] |
| `in-b16-128d-x500` | 1.806 [1.796,1.834] |
| `out-b16-128d-x500` | 3.505 [3.447,3.569] |
| `in-b16-256d-x500` | 1.872 [1.854,1.894] |
| `out-b16-256d-x500` | 3.384 [3.381,3.409] |
| `in-b16-1024d-x20` | 1.040 [1.030,1.046] |
| `out-b16-1024d-x20` | 2.320 [2.156,2.350] |
| `in-b16-10000d-x2` | 0.389 [0.389,0.391] |
| `out-b16-10000d-x2` | 1.513 [1.508,1.514] |
| `in-b16-100000d-x1` | 0.130 [0.128,0.131] |
| `out-b16-100000d-x1` | 1.307 [1.304,1.309] |

## holdout intervals

| Workload | DC / grouped, 95% paired interval |
| --- | --- |
| `in-b2-80d-x1000` | 2.202 [2.189,2.216] |
| `out-b2-80d-x1000` | 2.470 [2.457,2.495] |
| `in-b2-257d-x200` | 2.687 [2.667,2.723] |
| `out-b2-257d-x200` | 3.340 [3.274,3.385] |
| `in-b2-2048d-x10` | 1.560 [1.523,1.597] |
| `out-b2-2048d-x10` | 2.035 [2.027,2.046] |
| `in-b2-20000d-x1` | 0.655 [0.653,0.664] |
| `out-b2-20000d-x1` | 1.453 [1.448,1.459] |
| `in-b2-120000d-x1` | 0.237 [0.236,0.237] |
| `out-b2-120000d-x1` | 1.340 [1.338,1.348] |
| `in-b10-80d-x1000` | 0.998 [0.987,1.009] |
| `out-b10-80d-x1000` | 1.000 [0.994,1.008] |
| `in-b10-257d-x200` | 1.004 [0.985,1.026] |
| `out-b10-257d-x200` | 0.991 [0.986,1.012] |
| `in-b10-2048d-x10` | 0.976 [0.957,1.004] |
| `out-b10-2048d-x10` | 0.999 [0.923,1.020] |
| `in-b10-20000d-x1` | 1.003 [0.985,1.058] |
| `out-b10-20000d-x1` | 0.997 [0.961,1.021] |
| `in-b10-120000d-x1` | 1.001 [0.997,1.006] |
| `out-b10-120000d-x1` | 1.000 [0.994,1.010] |
| `in-b16-80d-x1000` | 2.173 [2.134,2.196] |
| `out-b16-80d-x1000` | 2.883 [2.873,2.902] |
| `in-b16-257d-x200` | 2.483 [2.432,2.508] |
| `out-b16-257d-x200` | 3.359 [3.348,3.378] |
| `in-b16-2048d-x10` | 0.777 [0.769,0.780] |
| `out-b16-2048d-x10` | 1.985 [1.966,1.994] |
| `in-b16-20000d-x1` | 0.285 [0.283,0.286] |
| `out-b16-20000d-x1` | 1.401 [1.399,1.405] |
| `in-b16-120000d-x1` | 0.123 [0.122,0.123] |
| `out-b16-120000d-x1` | 1.293 [1.292,1.297] |

## input-sweep intervals

| Workload | DC / grouped, 95% paired interval |
| --- | --- |
| `in-b2-32767d-x1` | 0.469 [0.447,0.469] |
| `in-b2-32768d-x1` | 0.468 [0.465,0.482] |
| `in-b2-32769d-x1` | 0.503 [0.500,0.527] |
| `in-b2-65535d-x1` | 0.323 [0.318,0.328] |
| `in-b2-65536d-x1` | 0.321 [0.320,0.322] |
| `in-b2-65537d-x1` | 0.347 [0.344,0.353] |
| `in-b3-32767d-x1` | 0.357 [0.354,0.378] |
| `in-b3-32768d-x1` | 0.356 [0.348,0.357] |
| `in-b3-32769d-x1` | 0.388 [0.379,0.389] |
| `in-b3-65535d-x1` | 0.256 [0.256,0.259] |
| `in-b3-65536d-x1` | 0.257 [0.255,0.260] |
| `in-b3-65537d-x1` | 0.282 [0.281,0.288] |
| `in-b8-32767d-x1` | 0.262 [0.261,0.265] |
| `in-b8-32768d-x1` | 0.263 [0.261,0.264] |
| `in-b8-32769d-x1` | 0.291 [0.288,0.296] |
| `in-b8-65535d-x1` | 0.198 [0.196,0.205] |
| `in-b8-65536d-x1` | 0.196 [0.195,0.198] |
| `in-b8-65537d-x1` | 0.218 [0.217,0.218] |
| `in-b16-32767d-x1` | 0.213 [0.212,0.217] |
| `in-b16-32768d-x1` | 0.213 [0.211,0.214] |
| `in-b16-32769d-x1` | 0.231 [0.231,0.232] |
| `in-b16-65535d-x1` | 0.155 [0.154,0.159] |
| `in-b16-65536d-x1` | 0.156 [0.155,0.156] |
| `in-b16-65537d-x1` | 0.170 [0.169,0.172] |

