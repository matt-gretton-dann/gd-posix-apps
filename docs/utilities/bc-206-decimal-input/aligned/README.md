# Revised decimal input and bounded Apple arm64 alignment

Candidate `2cd685cb4f228d639d40a83ebd271a3d61f63e7d` versus local accepted
`0868b4acd36d91a39a1d07c1aafb0808bda519cc`. Native base is `ad1b082`
(full SHA in each manifest), whose production arithmetic/VM sources match
0868b4a. The only later numeric source change is a guarded 64-byte function-
entry alignment hint for Apple arm64 Clang; all other platforms keep their
generic compiler policy. This does not introduce specialized arithmetic or
assert a hardware cause. Initial and scalar-stage regressions remain archived.

All 120 quick, 1200 full and 1200 held-out invocations pass exact output,
status, stderr and deadline checks. Nine seeded paired rounds follow one
warm-up. The separately pre-bounded division/modulo batches use 27 pairs.
Every raw row remains, including inconclusive intervals. No local compiler,
unit test or profiling workload overlaps these timings. Compiler/library,
Release flags, fixed split point 126, host and pinned Howard identities are
recorded in the manifests; this shared host has no thermal/frequency isolation.

The all-class full decimal-input ratio is 0.145; held-out remains near 0.13.
No full-set median is more than 3.5% slower. All-class held-out division is
1.036 [0.973,1.112]. The earlier 27-pair held-out division batch is 1.063
[1.049,1.078] and modulo 1.050 [1.037,1.058]. These remain inconclusive under
the documented 5% material-regression policy; no neutral-performance claim
or favorable-only replacement of the earlier batch is made.

Native run 41275892730 passes 1450 checked invocations on each OS with no
confirmed paired/A/A regressions. Decimal input ratios are Linux 0.048,
Windows 0.085 and macOS 0.151. macOS division 1.069 [0.954,1.120] and Windows
modulo remain inconclusive. Verified raw ZIPs and SHA-256 digests are included.

Final source adoption is conditional on green checks for the reviewed head.
Nondecimal grouping, buffered output and crossover decisions remain #206 work.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard (medians) |
| --- | --- | --- |
| `startup` | 0.974 [0.863, 1.228] | 1.057 |
| `loop_1000000` | 0.992 [0.987, 0.997] | 1.049 |
| `calls_200000` | 0.989 [0.987, 0.994] | 1.593 |
| `array_200000` | 0.991 [0.970, 1.005] | 1.169 |
| `compare_high_1000d_x100000` | 0.983 [0.956, 0.991] | 1.122 |
| `compare_low_1000d_x100000` | 0.993 [0.945, 1.013] | 1.245 |
| `compare_equal_1000d_x100000` | 1.000 [0.984, 1.017] | 1.251 |
| `fibonacci_1000_x100` | 0.991 [0.980, 0.996] | 0.822 |
| `fibonacci_10000_x10` | 0.994 [0.986, 1.001] | 0.998 |
| `factorial_1000_x50` | 0.999 [0.995, 1.018] | 0.869 |
| `factorial_5000_x5` | 1.000 [0.994, 1.006] | 0.749 |
| `multiply_100d_x100000` | 1.001 [0.992, 1.017] | 0.684 |
| `multiply_1000d_x10000` | 1.001 [0.996, 1.022] | 2.045 |
| `multiply_10000d_x1000` | 0.972 [0.971, 0.974] | 6.070 |
| `divide_scale100_x100000` | 0.996 [0.991, 0.999] | 0.746 |
| `divide_scale1000_x10000` | 0.995 [0.992, 1.006] | 0.667 |
| `divide_scale5000_x5000` | 0.999 [0.998, 1.015] | 0.617 |
| `sqrt_scale100_x5000` | 1.008 [1.001, 1.023] | 0.607 |
| `sqrt_scale1000_x50` | 1.000 [0.993, 1.002] | 0.901 |
| `sqrt_scale5000_x5` | 0.999 [0.995, 1.007] | 0.854 |
| `multiply_dense_1000d_x5000` | 1.002 [0.999, 1.004] | 1.016 |
| `multiply_dense_10000d_x500` | 0.953 [0.950, 0.955] | 0.776 |
| `multiply_unbalanced_10000d_100d_x2000` | 0.879 [0.874, 0.881] | 0.681 |
| `multiply_sparse_10000d_x1000` | 0.912 [0.907, 1.127] | 2.392 |
| `divide_multilimb_900d_450d_s100_x1000` | 0.991 [0.975, 1.038] | 0.875 |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.019 [0.992, 1.038] | 0.981 |
| `modulo_1000d_350d_x3000` | 0.999 [0.992, 1.005] | 0.708 |
| `power_100d_e17_x1000` | 0.996 [0.989, 1.019] | 0.503 |
| `power_20d_e32_x2000` | 0.997 [0.980, 1.007] | 0.521 |
| `ibase_literal_flip_20000` | 1.008 [0.968, 1.022] | 1.172 |
| `recursion_50_x1000` | 1.000 [0.982, 1.024] | 1.936 |
| `sparse_array_200000` | 0.996 [0.993, 1.005] | 0.990 |
| `frontend_definitions_2000` | 1.002 [0.998, 1.019] | 3.491 |
| `decimal_input_4000d_x100` | 0.145 [0.142, 0.146] | 5.083 |
| `decimal_output_200d_x1000` | 1.006 [0.996, 1.058] | 2.153 |
| `streaming_statements_20000` | 0.990 [0.968, 1.002] | 2.898 |
| `square_100d_x100000` | 0.994 [0.989, 0.997] | 0.530 |
| `square_1000d_x10000` | 0.997 [0.995, 1.003] | 0.484 |
| `square_10000d_x500` | 0.965 [0.960, 0.968] | 0.402 |
| `literal_repeat_2000d_x2000` | 0.897 [0.870, 0.924] | 0.929 |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard (medians) |
| --- | --- | --- |
| `startup` | 0.992 [0.850, 1.118] | 1.060 |
| `loop_875000` | 0.990 [0.977, 0.993] | 1.058 |
| `calls_175000` | 0.990 [0.987, 0.992] | 1.594 |
| `array_175000` | 0.993 [0.987, 0.997] | 1.161 |
| `compare_high_1125d_x87500` | 0.986 [0.971, 0.991] | 1.124 |
| `compare_low_1125d_x87500` | 1.004 [0.978, 1.019] | 1.233 |
| `compare_equal_1125d_x87500` | 1.001 [0.991, 1.018] | 1.255 |
| `fibonacci_1125_x88` | 0.988 [0.977, 0.995] | 0.825 |
| `fibonacci_11250_x9` | 1.002 [0.999, 1.020] | 1.017 |
| `factorial_1125_x44` | 0.993 [0.988, 1.008] | 0.863 |
| `factorial_5625_x5` | 0.996 [0.993, 1.001] | 0.740 |
| `multiply_112d_x87500` | 1.000 [0.990, 1.006] | 0.697 |
| `multiply_1125d_x8750` | 0.998 [0.995, 1.001] | 2.302 |
| `multiply_11250d_x875` | 0.977 [0.952, 0.979] | 6.772 |
| `divide_scale112_x87500` | 0.995 [0.970, 1.004] | 0.739 |
| `divide_scale1125_x8750` | 0.994 [0.990, 0.997] | 0.579 |
| `divide_scale5625_x4375` | 1.003 [0.990, 1.035] | 0.484 |
| `sqrt_scale112_x4375` | 1.007 [0.997, 1.009] | 0.637 |
| `sqrt_scale1125_x44` | 0.999 [0.997, 1.022] | 0.888 |
| `sqrt_scale5625_x5` | 1.000 [0.997, 1.007] | 0.851 |
| `multiply_dense_1125d_x4375` | 0.999 [0.987, 1.003] | 1.044 |
| `multiply_dense_11250d_x438` | 0.948 [0.941, 0.957] | 0.820 |
| `multiply_unbalanced_11250d_112d_x1750` | 0.853 [0.850, 0.856] | 0.621 |
| `multiply_sparse_11250d_x875` | 0.902 [0.900, 0.914] | 2.593 |
| `divide_multilimb_1012d_506d_s112_x875` | 1.036 [0.973, 1.112] | 0.663 |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.025 [1.011, 1.030] | 1.284 |
| `modulo_1125d_393d_x2625` | 1.033 [1.017, 1.064] | 0.560 |
| `power_112d_e17_x875` | 1.000 [0.986, 1.005] | 0.518 |
| `power_22d_e32_x1750` | 0.998 [0.990, 1.000] | 0.506 |
| `ibase_literal_flip_17500` | 1.010 [1.006, 1.050] | 1.185 |
| `recursion_56_x875` | 0.996 [0.970, 0.999] | 1.920 |
| `sparse_array_175000` | 0.995 [0.986, 1.007] | 0.985 |
| `frontend_definitions_1750` | 1.002 [0.997, 1.020] | 3.348 |
| `decimal_input_4500d_x88` | 0.130 [0.128, 0.131] | 5.144 |
| `decimal_output_225d_x875` | 0.999 [0.979, 1.064] | 2.203 |
| `streaming_statements_17500` | 1.002 [0.992, 1.011] | 2.964 |
| `square_112d_x87500` | 0.993 [0.987, 0.994] | 0.534 |
| `square_1125d_x8750` | 0.977 [0.973, 0.979] | 0.509 |
| `square_11250d_x438` | 0.959 [0.954, 0.960] | 0.414 |
| `literal_repeat_2250d_x1750` | 0.878 [0.840, 0.936] | 0.914 |
