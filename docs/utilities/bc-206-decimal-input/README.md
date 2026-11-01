# Decimal input experiment — acceptance pending

Initial source `dd88b5cec60fc0796bea11998f547084db11f0fd` exposed a one-digit
component regression. Revised source `bd002d4c671a91a6d08899f5e536155497c6be31`
restores a decimal single-digit scalar path. Both comparisons below use the
explicit historical accepted parent `a5c1cb1ad4051616afdce115b02ecc0247ad62f7`.
The revised source also includes main's profile-only scan counters (#324);
an exact current-main comparison is required before attributing the remaining
division/modulo differences to conversion. These are retained observations,
not evidence of neutral performance or an accepted cross-platform speedup.

All 360 revised component invocations pass exact result, checksum, status and
stderr checks. One-digit conversion improves from the initial 1.551 ratio to
0.941 [0.939, 0.981]. Other sizes retain their input-conversion gains. Internal
chrono intervals include reset/consumer work; control rows are separate and
are not subtracted. Process wall times are auxiliary, not pure kernel cost.

Each process stage passes 120 quick, 1200 full and 1200 held-out exact checks.
Full/held-out timing uses one checked warm-up and nine seeded paired rounds
with 2000 paired bootstrap resamples. No builds/tests/profiles overlap local
timing. Apple M1/macOS 27.2, Apple Clang 21/libc++ 220106, Release thin LTO,
fixed split point 126 and native pinned Howard 7.0.3; exact manifests and raw
rows are included. Shared-host frequency/thermal isolation is unavailable.

Revised held-out multi-limb division is 1.175 [1.135, 1.197] and modulo is
1.127 [1.118, 1.150] against the historical parent. Keep the PR on hold while
checking current-main attribution and bounded confirmation. Initial native run
43 covers the old PR subset, which omitted modulo and decimal-output groups;
its absence of confirmed regressions does not resolve those omissions. #325
now fixes the subset, and must be included in the final acceptance run.

Initial diagnostic counters retain their actual older source identities and
are not relabeled as the scalar revision. They are separate from timings.

## Initial full

| Workload | Candidate / historical parent [95%] |
| --- | --- |
| `startup` | 0.978 [0.849, 1.156] |
| `loop_1000000` | 0.998 [0.993, 1.000] |
| `calls_200000` | 1.006 [1.004, 1.012] |
| `array_200000` | 1.000 [0.990, 1.134] |
| `compare_high_1000d_x100000` | 0.988 [0.975, 1.023] |
| `compare_low_1000d_x100000` | 0.993 [0.985, 0.999] |
| `compare_equal_1000d_x100000` | 1.005 [0.986, 1.009] |
| `fibonacci_1000_x100` | 0.997 [0.988, 0.999] |
| `fibonacci_10000_x10` | 0.998 [0.970, 1.017] |
| `factorial_1000_x50` | 1.003 [0.996, 1.018] |
| `factorial_5000_x5` | 0.997 [0.993, 1.002] |
| `multiply_100d_x100000` | 1.000 [0.996, 1.004] |
| `multiply_1000d_x10000` | 0.998 [0.997, 1.006] |
| `multiply_10000d_x1000` | 0.982 [0.964, 0.983] |
| `divide_scale100_x100000` | 0.994 [0.983, 0.999] |
| `divide_scale1000_x10000` | 0.995 [0.988, 1.001] |
| `divide_scale5000_x5000` | 1.003 [1.000, 1.014] |
| `sqrt_scale100_x5000` | 1.009 [0.996, 1.014] |
| `sqrt_scale1000_x50` | 0.997 [0.987, 1.002] |
| `sqrt_scale5000_x5` | 0.998 [0.996, 1.001] |
| `multiply_dense_1000d_x5000` | 0.998 [0.987, 1.004] |
| `multiply_dense_10000d_x500` | 0.960 [0.955, 0.964] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.945 [0.894, 0.947] |
| `multiply_sparse_10000d_x1000` | 0.910 [0.876, 0.913] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.065 [1.050, 1.080] |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.026 [1.017, 1.033] |
| `modulo_1000d_350d_x3000` | 1.011 [1.002, 1.025] |
| `power_100d_e17_x1000` | 0.996 [0.993, 1.003] |
| `power_20d_e32_x2000` | 1.001 [0.985, 1.011] |
| `ibase_literal_flip_20000` | 1.012 [0.994, 1.023] |
| `recursion_50_x1000` | 1.001 [0.994, 1.029] |
| `sparse_array_200000` | 0.999 [0.989, 1.002] |
| `frontend_definitions_2000` | 0.998 [0.989, 1.012] |
| `decimal_input_4000d_x100` | 0.143 [0.141, 0.144] |
| `decimal_output_200d_x1000` | 1.004 [0.988, 1.050] |
| `streaming_statements_20000` | 0.992 [0.986, 1.009] |
| `square_100d_x100000` | 0.999 [0.990, 1.002] |
| `square_1000d_x10000` | 0.999 [0.997, 1.001] |
| `square_10000d_x500` | 0.961 [0.956, 0.963] |
| `literal_repeat_2000d_x2000` | 0.907 [0.852, 0.946] |

## Initial holdout

| Workload | Candidate / historical parent [95%] |
| --- | --- |
| `startup` | 0.976 [0.890, 1.235] |
| `loop_875000` | 0.998 [0.997, 1.000] |
| `calls_175000` | 1.008 [0.994, 1.014] |
| `array_175000` | 1.003 [0.994, 1.009] |
| `compare_high_1125d_x87500` | 0.982 [0.963, 0.994] |
| `compare_low_1125d_x87500` | 0.988 [0.976, 1.006] |
| `compare_equal_1125d_x87500` | 0.994 [0.985, 1.005] |
| `fibonacci_1125_x88` | 0.993 [0.987, 1.001] |
| `fibonacci_11250_x9` | 1.004 [0.994, 1.009] |
| `factorial_1125_x44` | 1.000 [0.978, 1.011] |
| `factorial_5625_x5` | 0.998 [0.996, 1.003] |
| `multiply_112d_x87500` | 0.999 [0.986, 1.004] |
| `multiply_1125d_x8750` | 1.001 [0.998, 1.002] |
| `multiply_11250d_x875` | 0.978 [0.936, 0.983] |
| `divide_scale112_x87500` | 0.992 [0.980, 0.996] |
| `divide_scale1125_x8750` | 1.008 [0.991, 1.058] |
| `divide_scale5625_x4375` | 1.008 [0.991, 1.032] |
| `sqrt_scale112_x4375` | 1.005 [0.999, 1.024] |
| `sqrt_scale1125_x44` | 0.998 [0.995, 1.003] |
| `sqrt_scale5625_x5` | 1.000 [0.996, 1.003] |
| `multiply_dense_1125d_x4375` | 0.999 [0.994, 1.003] |
| `multiply_dense_11250d_x438` | 0.956 [0.954, 0.959] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.854 [0.850, 0.861] |
| `multiply_sparse_11250d_x875` | 0.902 [0.898, 0.906] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.140 [1.032, 1.175] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.033 [1.012, 1.052] |
| `modulo_1125d_393d_x2625` | 1.088 [1.054, 1.113] |
| `power_112d_e17_x875` | 1.001 [0.985, 1.007] |
| `power_22d_e32_x1750` | 0.999 [0.989, 1.008] |
| `ibase_literal_flip_17500` | 1.022 [1.009, 1.033] |
| `recursion_56_x875` | 1.000 [0.986, 1.014] |
| `sparse_array_175000` | 0.999 [0.993, 1.009] |
| `frontend_definitions_1750` | 1.005 [0.983, 1.035] |
| `decimal_input_4500d_x88` | 0.129 [0.128, 0.131] |
| `decimal_output_225d_x875` | 0.989 [0.973, 1.025] |
| `streaming_statements_17500` | 0.995 [0.990, 0.996] |
| `square_112d_x87500` | 0.994 [0.970, 1.001] |
| `square_1125d_x8750` | 1.001 [0.996, 1.012] |
| `square_11250d_x438` | 0.956 [0.952, 0.960] |
| `literal_repeat_2250d_x1750` | 0.868 [0.860, 0.940] |

## Scalar full

| Workload | Candidate / historical parent [95%] |
| --- | --- |
| `startup` | 0.965 [0.907, 1.092] |
| `loop_1000000` | 0.997 [0.992, 1.020] |
| `calls_200000` | 1.019 [0.979, 1.022] |
| `array_200000` | 1.001 [0.992, 1.005] |
| `compare_high_1000d_x100000` | 1.003 [0.976, 1.011] |
| `compare_low_1000d_x100000` | 1.001 [0.979, 1.026] |
| `compare_equal_1000d_x100000` | 0.994 [0.982, 1.016] |
| `fibonacci_1000_x100` | 1.000 [0.993, 1.008] |
| `fibonacci_10000_x10` | 0.999 [0.993, 1.004] |
| `factorial_1000_x50` | 0.999 [0.997, 1.002] |
| `factorial_5000_x5` | 0.999 [0.987, 1.001] |
| `multiply_100d_x100000` | 1.006 [0.990, 1.019] |
| `multiply_1000d_x10000` | 0.999 [0.994, 1.003] |
| `multiply_10000d_x1000` | 0.974 [0.967, 0.975] |
| `divide_scale100_x100000` | 0.994 [0.988, 1.000] |
| `divide_scale1000_x10000` | 1.000 [0.982, 1.019] |
| `divide_scale5000_x5000` | 0.997 [0.980, 1.009] |
| `sqrt_scale100_x5000` | 1.006 [0.999, 1.014] |
| `sqrt_scale1000_x50` | 1.000 [0.998, 1.005] |
| `sqrt_scale5000_x5` | 1.000 [0.997, 1.007] |
| `multiply_dense_1000d_x5000` | 1.000 [0.998, 1.004] |
| `multiply_dense_10000d_x500` | 0.955 [0.944, 0.963] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.947 [0.941, 0.978] |
| `multiply_sparse_10000d_x1000` | 0.911 [0.907, 0.920] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.092 [1.023, 1.167] |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.035 [1.025, 1.049] |
| `modulo_1000d_350d_x3000` | 1.056 [1.017, 1.059] |
| `power_100d_e17_x1000` | 1.003 [0.987, 1.007] |
| `power_20d_e32_x2000` | 1.006 [0.973, 1.046] |
| `ibase_literal_flip_20000` | 1.028 [1.011, 1.073] |
| `recursion_50_x1000` | 0.999 [0.969, 1.018] |
| `sparse_array_200000` | 1.005 [0.973, 1.042] |
| `frontend_definitions_2000` | 1.000 [0.994, 1.013] |
| `decimal_input_4000d_x100` | 0.143 [0.142, 0.144] |
| `decimal_output_200d_x1000` | 1.011 [0.990, 1.031] |
| `streaming_statements_20000` | 0.990 [0.960, 1.017] |
| `square_100d_x100000` | 0.996 [0.990, 0.998] |
| `square_1000d_x10000` | 0.998 [0.993, 1.009] |
| `square_10000d_x500` | 0.961 [0.938, 0.964] |
| `literal_repeat_2000d_x2000` | 0.899 [0.860, 0.918] |

## Scalar holdout

| Workload | Candidate / historical parent [95%] |
| --- | --- |
| `startup` | 0.964 [0.954, 1.144] |
| `loop_875000` | 1.006 [1.002, 1.012] |
| `calls_175000` | 1.014 [1.007, 1.028] |
| `array_175000` | 1.013 [1.004, 1.024] |
| `compare_high_1125d_x87500` | 0.992 [0.991, 0.995] |
| `compare_low_1125d_x87500` | 1.001 [0.984, 1.009] |
| `compare_equal_1125d_x87500` | 1.003 [0.976, 1.018] |
| `fibonacci_1125_x88` | 0.998 [0.991, 1.005] |
| `fibonacci_11250_x9` | 0.999 [0.997, 1.008] |
| `factorial_1125_x44` | 0.998 [0.992, 1.004] |
| `factorial_5625_x5` | 0.999 [0.994, 1.006] |
| `multiply_112d_x87500` | 1.001 [0.990, 1.004] |
| `multiply_1125d_x8750` | 1.000 [0.996, 1.030] |
| `multiply_11250d_x875` | 0.978 [0.974, 1.003] |
| `divide_scale112_x87500` | 0.998 [0.984, 1.009] |
| `divide_scale1125_x8750` | 0.996 [0.985, 1.020] |
| `divide_scale5625_x4375` | 0.999 [0.973, 1.016] |
| `sqrt_scale112_x4375` | 1.003 [0.980, 1.007] |
| `sqrt_scale1125_x44` | 0.997 [0.988, 1.008] |
| `sqrt_scale5625_x5` | 0.998 [0.991, 1.008] |
| `multiply_dense_1125d_x4375` | 1.000 [0.995, 1.002] |
| `multiply_dense_11250d_x438` | 0.954 [0.946, 0.959] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.857 [0.854, 0.865] |
| `multiply_sparse_11250d_x875` | 0.902 [0.896, 0.903] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.174 [1.135, 1.197] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.030 [0.991, 1.048] |
| `modulo_1125d_393d_x2625` | 1.127 [1.118, 1.150] |
| `power_112d_e17_x875` | 1.004 [0.979, 1.007] |
| `power_22d_e32_x1750` | 0.999 [0.966, 1.011] |
| `ibase_literal_flip_17500` | 1.020 [0.996, 1.067] |
| `recursion_56_x875` | 0.998 [0.959, 1.009] |
| `sparse_array_175000` | 1.004 [0.996, 1.026] |
| `frontend_definitions_1750` | 1.002 [0.985, 1.019] |
| `decimal_input_4500d_x88` | 0.129 [0.128, 0.132] |
| `decimal_output_225d_x875` | 1.000 [0.992, 1.066] |
| `streaming_statements_17500` | 1.003 [0.984, 1.018] |
| `square_112d_x87500` | 0.999 [0.965, 1.007] |
| `square_1125d_x8750` | 0.980 [0.977, 0.985] |
| `square_11250d_x438` | 0.959 [0.956, 0.963] |
| `literal_repeat_2250d_x1750` | 0.877 [0.866, 0.894] |

## Revised components

| Component | Candidate / historical parent [95%] |
| --- | --- |
| `control-1d-x100000` | 1.000 [0.995, 1.000] |
| `conversion-in-1d-x100000` | 0.941 [0.939, 0.981] |
| `conversion-out-1d-x100000` | 1.000 [0.999, 1.028] |
| `control-9d-x100000` | 1.000 [0.997, 1.000] |
| `conversion-in-9d-x100000` | 0.511 [0.508, 0.516] |
| `conversion-out-9d-x100000` | 1.001 [1.000, 1.039] |
| `control-10d-x100000` | 1.000 [1.000, 1.065] |
| `conversion-in-10d-x100000` | 0.386 [0.383, 0.390] |
| `conversion-out-10d-x100000` | 0.999 [0.996, 1.017] |
| `control-100d-x10000` | 1.000 [0.999, 1.001] |
| `conversion-in-100d-x10000` | 0.116 [0.116, 0.117] |
| `conversion-out-100d-x10000` | 0.998 [0.994, 1.006] |
| `control-1000d-x500` | 0.983 [0.938, 1.000] |
| `conversion-in-1000d-x500` | 0.020 [0.020, 0.020] |
| `conversion-out-1000d-x500` | 0.999 [0.998, 1.011] |
| `control-10000d-x5` | 1.000 [0.636, 2.000] |
| `conversion-in-10000d-x5` | 0.002 [0.002, 0.002] |
| `conversion-out-10000d-x5` | 0.996 [0.993, 1.005] |
