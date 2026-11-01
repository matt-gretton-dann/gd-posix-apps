# Portable triangular squares — #203

Identical coefficient spans use a triangular schoolbook square. Diagonals are
computed once, cross terms twice, with a compile-time widened accumulator bound.
Recursive low/high squares retain their shared inputs; the middle square reuses
one sum. Single-limb multiply-accumulate and unsupported-trait generic fallbacks
remain. Sign and decimal scale stay in Number; equality of values is not scanned
to discover squares.

Parent `58075b41a293a7d3f4cfbad308ff75997702e496` has identical production C++/CMake
as accepted main `a520f2a0db29858643dd0d24eb2470329d56ab56`. Candidate is
`61d3a9ce4c487ec504f83bfbbd1f389c6a73214e`. Both immutable recipes use fixed
126, Release -O3/-DNDEBUG, thin LTO, Apple Clang 21/libc++ header 220106 on the
recorded Apple M1/macOS 27.2 host. Manifests retain actual commands and binary
hashes. Pinned native Howard 7.0.3 is the competitive reference. This experiment
uses no instrumentation, PGO or local concurrent builds/tests/profiles.

All 1,170 full and 1,170 held-out invocations pass exact independent output,
status, stderr and deadline gates. Each workload has one checked warm-up and
nine measured paired rounds, interleaved in seeded order with 2,000 bootstrap
resamples. Full seed 197200 and held-out seed 197201/dimensions remain distinct.
The 39-case generator appends three square workloads without changing the older
36 seeded operands. No aggregate or exclusions hide losing classes. This shared
workstation has no frequency/thermal isolation; intervals are sample evidence,
not guarantees for other machines. Hosted native results are reported separately.

Combined correctness: 163 Release bc cases/583,372 assertions; 123 extensions-
disabled ASan/UBSan number/VM cases/399,163 assertions; six profiling checks and
eight CLI groups pass. Independent decimal convolution covers all three limb
widths, exact output guards, carries, sparse/zero inputs, aliases, opposite signs,
scales and threshold boundaries. The component driver adds an aliased square
with an independent Python integer oracle. #211 product counts remain checked.

#203 remains open for asymmetric dispatch, robust sweeps and stable defaults.

## Dispatch decision and limitations

The [initial experiment](initial-in-body/README.md) recognized squares inside
ordinary schoolbook multiplication. Native Linux run 31 confirmed a 5.9%
regression in the existing small product. Its complete local and native data
remain archived. The revised candidate recognizes squares in dispatch and
restores the ordinary schoolbook body exactly to the accepted parent. This is
a measured revision; compiler/code-layout causality has not been established.

Revised local small multiplication is 1.005 [0.996, 1.024] on full dimensions.
Full squares take 0.782/0.466/0.526 of parent time at 100/1000/10000 digits;
held-out squares take 0.789/0.493/0.515. Full division at 900/450 digits is
1.062 [1.004, 1.114], crossing the provisional 5% bound and remaining
inconclusive. All held-out medians are below 1.04. Neutral/losing classes and
Howard gaps remain visible in every table.

Native run 32 uses exact accepted main a520f2a and candidate 61d3a9c. Verified
Linux/macOS/Windows artifacts each contain 1,050 checked invocations with no
confirmed paired/A/A regression. Linux small product is 1.024 [1.020, 1.030].
macOS small product 1.111 [0.996, 1.181] and Windows 1.011 [0.990, 1.057]
remain explicitly inconclusive; these are not universal no-regression results.
Small/large squares take Linux 0.865/0.546, macOS 0.752/0.515 and Windows
0.845/0.541 of parent time. Startup is still uncalibrated. Raw ZIPs, digests and
exact source identities are retained next to this report.

All 78 separate diagnostic invocations pass and balance selected storage at
EOF. At 1000 digits, 10,000 basic products become 10,000 triangular squares.
At 10000 digits, 40,500 basic leaves become triangular squares while 20,000
Karatsuba calls and 1,531 selected allocations remain unchanged. Reusing the
middle sum avoids arithmetic without claiming a new allocation reduction.
Diagnostics are not timed. A nondecimal base-200 test model proves that the
ordinary multiplication fallback compiles when a doubled product would exceed
the widened type; all three normal decimal traits exercise the square path.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.984 [0.921, 1.019] | 1.009 |
| `loop_1000000` | 0.991 [0.981, 1.000] | 1.303 |
| `calls_200000` | 1.000 [0.995, 1.011] | 1.740 |
| `array_200000` | 0.998 [0.981, 1.034] | 1.337 |
| `compare_high_1000d_x100000` | 1.001 [0.988, 1.013] | 1.285 |
| `compare_low_1000d_x100000` | 1.003 [0.987, 1.030] | 1.407 |
| `compare_equal_1000d_x100000` | 1.000 [0.992, 1.024] | 1.386 |
| `fibonacci_1000_x100` | 1.004 [0.998, 1.012] | 0.844 |
| `fibonacci_10000_x10` | 1.001 [0.998, 1.006] | 1.010 |
| `factorial_1000_x50` | 1.000 [0.980, 1.000] | 0.878 |
| `factorial_5000_x5` | 0.999 [0.997, 1.004] | 0.750 |
| `multiply_100d_x100000` | 1.005 [0.996, 1.024] | 0.737 |
| `multiply_1000d_x10000` | 1.001 [0.999, 1.002] | 2.055 |
| `multiply_10000d_x1000` | 1.000 [0.997, 1.003] | 6.176 |
| `divide_scale100_x100000` | 0.998 [0.991, 1.021] | 0.937 |
| `divide_scale1000_x10000` | 0.994 [0.969, 1.002] | 0.682 |
| `divide_scale5000_x5000` | 1.000 [0.968, 1.007] | 0.622 |
| `sqrt_scale100_x5000` | 1.003 [0.998, 1.008] | 0.604 |
| `sqrt_scale1000_x50` | 1.000 [0.994, 1.009] | 0.900 |
| `sqrt_scale5000_x5` | 1.006 [0.998, 1.024] | 0.856 |
| `multiply_dense_1000d_x5000` | 0.999 [0.995, 1.004] | 1.015 |
| `multiply_dense_10000d_x500` | 1.000 [0.999, 1.005] | 0.808 |
| `multiply_unbalanced_10000d_100d_x2000` | 0.997 [0.940, 1.001] | 0.733 |
| `multiply_sparse_10000d_x1000` | 1.001 [0.996, 1.006] | 2.630 |
| `divide_multilimb_900d_450d_s100_x1000` | 1.062 [1.004, 1.114] | 0.930 |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.001 [0.998, 1.011] | 0.951 |
| `modulo_1000d_350d_x3000` | 1.005 [0.999, 1.039] | 0.712 |
| `power_100d_e17_x1000` | 0.622 [0.618, 0.624] | 0.504 |
| `power_20d_e32_x2000` | 0.755 [0.747, 0.771] | 0.529 |
| `ibase_literal_flip_20000` | 0.997 [0.984, 1.016] | 1.210 |
| `recursion_50_x1000` | 1.003 [0.984, 1.010] | 2.011 |
| `sparse_array_200000` | 0.999 [0.991, 1.008] | 1.148 |
| `frontend_definitions_2000` | 1.001 [0.971, 1.004] | 3.472 |
| `decimal_input_4000d_x100` | 0.996 [0.990, 0.999] | 33.965 |
| `decimal_output_200d_x1000` | 0.994 [0.949, 1.022] | 2.121 |
| `streaming_statements_20000` | 1.004 [0.995, 1.014] | 2.949 |
| `square_100d_x100000` | 0.782 [0.743, 0.792] | 0.582 |
| `square_1000d_x10000` | 0.466 [0.464, 0.467] | 0.481 |
| `square_10000d_x500` | 0.526 [0.525, 0.527] | 0.417 |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.999 [0.896, 1.067] | 1.020 |
| `loop_875000` | 0.980 [0.971, 0.993] | 1.276 |
| `calls_175000` | 0.999 [0.991, 1.019] | 1.740 |
| `array_175000` | 0.987 [0.931, 0.998] | 1.349 |
| `compare_high_1125d_x87500` | 0.996 [0.961, 1.033] | 1.273 |
| `compare_low_1125d_x87500` | 1.012 [0.980, 1.022] | 1.368 |
| `compare_equal_1125d_x87500` | 0.985 [0.974, 1.010] | 1.344 |
| `fibonacci_1125_x88` | 1.009 [0.998, 1.037] | 0.853 |
| `fibonacci_11250_x9` | 0.998 [0.994, 1.005] | 1.027 |
| `factorial_1125_x44` | 0.999 [0.979, 1.003] | 0.865 |
| `factorial_5625_x5` | 0.997 [0.993, 1.003] | 0.743 |
| `multiply_112d_x87500` | 1.000 [0.990, 1.009] | 0.731 |
| `multiply_1125d_x8750` | 1.001 [0.988, 1.007] | 2.310 |
| `multiply_11250d_x875` | 1.004 [0.998, 1.006] | 6.922 |
| `divide_scale112_x87500` | 0.993 [0.934, 1.023] | 0.830 |
| `divide_scale1125_x8750` | 1.000 [0.992, 1.024] | 0.592 |
| `divide_scale5625_x4375` | 1.002 [0.947, 1.008] | 0.483 |
| `sqrt_scale112_x4375` | 1.004 [0.987, 1.013] | 0.635 |
| `sqrt_scale1125_x44` | 1.003 [1.001, 1.006] | 0.889 |
| `sqrt_scale5625_x5` | 0.998 [0.868, 1.002] | 0.851 |
| `multiply_dense_1125d_x4375` | 1.002 [0.998, 1.004] | 1.050 |
| `multiply_dense_11250d_x438` | 1.000 [0.999, 1.002] | 0.860 |
| `multiply_unbalanced_11250d_112d_x1750` | 0.999 [0.994, 1.004] | 0.721 |
| `multiply_sparse_11250d_x875` | 0.999 [0.993, 1.003] | 2.865 |
| `divide_multilimb_1012d_506d_s112_x875` | 1.005 [0.955, 1.038] | 0.610 |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.002 [0.998, 1.013] | 1.251 |
| `modulo_1125d_393d_x2625` | 0.999 [0.997, 1.008] | 0.534 |
| `power_112d_e17_x875` | 0.617 [0.611, 0.635] | 0.511 |
| `power_22d_e32_x1750` | 0.698 [0.665, 0.744] | 0.508 |
| `ibase_literal_flip_17500` | 0.983 [0.962, 1.013] | 1.165 |
| `recursion_56_x875` | 0.993 [0.988, 1.019] | 1.978 |
| `sparse_array_175000` | 0.994 [0.986, 1.009] | 1.136 |
| `frontend_definitions_1750` | 0.999 [0.967, 1.010] | 3.308 |
| `decimal_input_4500d_x88` | 0.996 [0.992, 1.006] | 35.734 |
| `decimal_output_225d_x875` | 1.002 [0.994, 1.034] | 2.156 |
| `streaming_statements_17500` | 1.003 [0.994, 1.012] | 2.941 |
| `square_112d_x87500` | 0.789 [0.776, 0.793] | 0.570 |
| `square_1125d_x8750` | 0.493 [0.491, 0.596] | 0.511 |
| `square_11250d_x438` | 0.515 [0.513, 0.518] | 0.432 |
