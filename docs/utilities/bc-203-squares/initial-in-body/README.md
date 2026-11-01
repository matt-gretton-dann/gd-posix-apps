# Initial square recognition inside the schoolbook body — #203

**Not accepted for merge.** Native Linux run 31 (1,140 checked invocations) confirms a regression in the
existing 100-digit nonsquare product: 1.059 [1.0501, 1.0663], after bounded
confirmation. Faster squares and passing local data do not supersede this
result. macOS and Windows each pass 1,050 checked invocations without a
confirmed regression. All three verified native ZIPs and their hashes are
retained in this directory. A separate revision moves recognition out of the schoolbook body;
compiler/code-layout causality has not been established.


Identical coefficient spans use a triangular schoolbook square. Diagonals are
computed once, cross terms twice, with a compile-time widened accumulator bound.
Recursive low/high squares retain their shared inputs; the middle square reuses
one sum. Single-limb multiply-accumulate and unsupported-trait generic fallbacks
remain. Sign and decimal scale stay in Number; equality of values is not scanned
to discover squares.

Parent `58075b41a293a7d3f4cfbad308ff75997702e496` has identical production C++/CMake
as accepted main `a520f2a0db29858643dd0d24eb2470329d56ab56`. Candidate is
`e127b8a11e35e6e031448c21c4577a57c1b8e5a5`. Both immutable recipes use fixed
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

Combined correctness: 162 Release bc cases/583,369 assertions; 122 extensions-
disabled ASan/UBSan number/VM cases/399,160 assertions; six profiling checks and
eight CLI groups pass. Independent decimal convolution covers all three limb
widths, exact output guards, carries, sparse/zero inputs, aliases, opposite signs,
scales and threshold boundaries. The component driver adds an aliased square
with an independent Python integer oracle. #211 product counts remain checked.

#203 remains open for asymmetric dispatch, robust sweeps and stable defaults.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.992 [0.955, 1.032] | 1.043 |
| `loop_1000000` | 0.990 [0.934, 0.993] | 1.314 |
| `calls_200000` | 1.002 [0.967, 1.025] | 1.741 |
| `array_200000` | 0.997 [0.985, 1.012] | 1.348 |
| `compare_high_1000d_x100000` | 1.009 [0.982, 1.023] | 1.300 |
| `compare_low_1000d_x100000` | 1.010 [0.979, 1.028] | 1.381 |
| `compare_equal_1000d_x100000` | 0.996 [0.988, 1.024] | 1.367 |
| `fibonacci_1000_x100` | 1.003 [1.000, 1.027] | 0.848 |
| `fibonacci_10000_x10` | 0.997 [0.992, 1.003] | 1.013 |
| `factorial_1000_x50` | 0.996 [0.970, 1.014] | 0.874 |
| `factorial_5000_x5` | 1.003 [0.992, 1.006] | 0.752 |
| `multiply_100d_x100000` | 0.997 [0.984, 1.003] | 0.733 |
| `multiply_1000d_x10000` | 1.000 [0.991, 1.001] | 2.054 |
| `multiply_10000d_x1000` | 1.001 [0.995, 1.005] | 6.198 |
| `divide_scale100_x100000` | 0.998 [0.994, 1.001] | 0.929 |
| `divide_scale1000_x10000` | 1.005 [0.997, 1.007] | 0.688 |
| `divide_scale5000_x5000` | 1.002 [0.994, 1.014] | 0.621 |
| `sqrt_scale100_x5000` | 1.003 [0.993, 1.007] | 0.604 |
| `sqrt_scale1000_x50` | 1.001 [0.991, 1.003] | 0.902 |
| `sqrt_scale5000_x5` | 0.999 [0.982, 1.005] | 0.853 |
| `multiply_dense_1000d_x5000` | 0.997 [0.994, 0.999] | 1.011 |
| `multiply_dense_10000d_x500` | 0.999 [0.993, 1.002] | 0.809 |
| `multiply_unbalanced_10000d_100d_x2000` | 0.991 [0.981, 0.998] | 0.727 |
| `multiply_sparse_10000d_x1000` | 1.002 [0.995, 1.005] | 2.632 |
| `divide_multilimb_900d_450d_s100_x1000` | 1.080 [0.964, 1.090] | 0.923 |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.007 [0.997, 1.016] | 0.945 |
| `modulo_1000d_350d_x3000` | 1.006 [1.002, 1.023] | 0.716 |
| `power_100d_e17_x1000` | 0.621 [0.616, 0.624] | 0.504 |
| `power_20d_e32_x2000` | 0.761 [0.742, 0.773] | 0.525 |
| `ibase_literal_flip_20000` | 1.007 [0.956, 1.020] | 1.190 |
| `recursion_50_x1000` | 1.003 [0.996, 1.012] | 2.003 |
| `sparse_array_200000` | 0.993 [0.987, 0.997] | 1.137 |
| `frontend_definitions_2000` | 1.007 [0.970, 1.014] | 3.522 |
| `decimal_input_4000d_x100` | 0.994 [0.974, 0.997] | 32.592 |
| `decimal_output_200d_x1000` | 1.004 [0.989, 1.049] | 2.131 |
| `streaming_statements_20000` | 1.002 [0.977, 1.010] | 2.995 |
| `square_100d_x100000` | 0.766 [0.751, 0.780] | 0.581 |
| `square_1000d_x10000` | 0.471 [0.468, 0.472] | 0.487 |
| `square_10000d_x500` | 0.526 [0.524, 0.528] | 0.417 |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.977 [0.959, 1.027] | 0.988 |
| `loop_875000` | 0.983 [0.979, 0.988] | 1.285 |
| `calls_175000` | 0.997 [0.989, 1.004] | 1.743 |
| `array_175000` | 0.995 [0.988, 1.021] | 1.342 |
| `compare_high_1125d_x87500` | 1.001 [0.999, 1.015] | 1.275 |
| `compare_low_1125d_x87500` | 1.001 [0.990, 1.023] | 1.360 |
| `compare_equal_1125d_x87500` | 0.992 [0.978, 1.050] | 1.347 |
| `fibonacci_1125_x88` | 1.002 [0.989, 1.017] | 0.854 |
| `fibonacci_11250_x9` | 1.008 [0.991, 1.011] | 1.034 |
| `factorial_1125_x44` | 0.997 [0.980, 1.000] | 0.860 |
| `factorial_5625_x5` | 0.997 [0.995, 1.002] | 0.742 |
| `multiply_112d_x87500` | 0.994 [0.992, 1.004] | 0.739 |
| `multiply_1125d_x8750` | 1.002 [0.996, 1.010] | 2.314 |
| `multiply_11250d_x875` | 1.002 [0.999, 1.013] | 6.888 |
| `divide_scale112_x87500` | 0.992 [0.971, 1.017] | 0.826 |
| `divide_scale1125_x8750` | 1.003 [0.962, 1.007] | 0.594 |
| `divide_scale5625_x4375` | 1.006 [0.994, 1.014] | 0.486 |
| `sqrt_scale112_x4375` | 1.002 [0.990, 1.008] | 0.636 |
| `sqrt_scale1125_x44` | 1.003 [0.999, 1.011] | 0.892 |
| `sqrt_scale5625_x5` | 1.000 [0.992, 1.010] | 0.851 |
| `multiply_dense_1125d_x4375` | 0.997 [0.993, 1.000] | 1.042 |
| `multiply_dense_11250d_x438` | 1.001 [0.997, 1.032] | 0.861 |
| `multiply_unbalanced_11250d_112d_x1750` | 1.008 [1.000, 1.014] | 0.725 |
| `multiply_sparse_11250d_x875` | 1.000 [0.994, 1.012] | 2.868 |
| `divide_multilimb_1012d_506d_s112_x875` | 1.003 [0.982, 1.173] | 0.620 |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.002 [0.990, 1.006] | 1.248 |
| `modulo_1125d_393d_x2625` | 1.007 [0.977, 1.054] | 0.542 |
| `power_112d_e17_x875` | 0.614 [0.611, 0.626] | 0.518 |
| `power_22d_e32_x1750` | 0.689 [0.669, 0.697] | 0.510 |
| `ibase_literal_flip_17500` | 0.998 [0.909, 1.011] | 1.193 |
| `recursion_56_x875` | 1.003 [0.990, 1.027] | 1.997 |
| `sparse_array_175000` | 0.989 [0.982, 0.993] | 1.138 |
| `frontend_definitions_1750` | 1.010 [0.971, 1.017] | 3.364 |
| `decimal_input_4500d_x88` | 0.996 [0.994, 0.997] | 38.298 |
| `decimal_output_225d_x875` | 1.000 [0.990, 1.018] | 2.127 |
| `streaming_statements_17500` | 1.001 [0.999, 1.008] | 2.927 |
| `square_112d_x87500` | 0.786 [0.781, 0.797] | 0.572 |
| `square_1125d_x8750` | 0.492 [0.487, 0.493] | 0.510 |
| `square_11250d_x438` | 0.517 [0.516, 0.535] | 0.433 |
