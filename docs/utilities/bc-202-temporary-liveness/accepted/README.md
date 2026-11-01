# Accepted arithmetic temporary consumption

Refs #202, #207 and #197; accepted by PR #342. Baseline
`e53a4b9fb73fc037bc4e30aecb310fbff37b92a2`; candidate
`7974cb3d8b16c98441f9dc960f6c91f52736b06d`.

Only proven heap-backed first operands transfer. Inline values copy, and outer
planning skips registered function bodies. The initial candidate and its costs
remain separately under `../initial`; no source identity is substituted.

## Matched local measurements

Apple M1, macOS 27.2, Apple Clang 21, libc++ 220106, Release thin LTO, split 126,
extensions ON. Full and held-out datasets each check 900 process invocations:
45 workloads, two roles, one warmup and nine shuffled paired samples. Every
output, exit status, stderr and deadline check passes. Corpus seeds 197200/197201
and schedule seeds 197227/197228 are separate; 2000 bootstrap resamples.
Profiling is OFF. Builds, tests, profiles and RSS diagnostics do not overlap
timings. Host scheduling, frequency and thermals remain uncontrolled.

Temporary-chain ratios are 0.8648 [0.8609,0.8682] full and 0.8747
[0.8727,0.8817] held-out. Scalar loop/call/definition medians retain roughly
1.6–2.5% local costs. No local class has CI95 wholly above 1.05; this is not
universal neutrality. All intervals, including losses, are retained below.

## Separate counters, memory and native checks

Ninety instrumented executions pass exact output and balanced storage checks.
Chain COW clones fall from 350000 to 50000 and copied limb bytes from
156800000 to 22400000, with unchanged VM instruction counts. The candidate
performs 350000 heap transfers. Scalar loop/calls perform no transfers.
Definitions now use 2000 plan allocations/28000 selected bytes, compared with
4000/60000 in the initial policy. Analysis vectors and owner headers are outside
selected allocator payload counts. Profile elapsed times are not speed evidence.

All 270 separate RSS executions (three per case/role) pass exact process/output
checks. Local getrusage maximum RSS is in bytes; its reference and raw resource
logs are retained. No general process-memory reduction is claimed. Loop medians
are 1966080/1966080 bytes; calls 1998848/2015232, definitions 2375680/2441216,
dense arrays 24264704/24264704, and chain 2064384/2080768. These three samples
do not establish portable peaks or confidence intervals. Both Mach-O __TEXT
segments are 311296 bytes; file sizes are 507776/508048. __TEXT is not RSS.

All fourteen CI/native/CodeQL checks pass at the exact candidate. Digest-checked
native ZIPs retain 1700 checked rows per Linux/macOS/Windows artifact, 5100 total,
with no confirmed regression under the documented policy. Chain ratios are
0.906/0.800/0.848. Linux definitions remain inconclusive at 1.046
[1.03365,1.05712]; several macOS scalar intervals are also noisy. Full raw
intervals, controls, source/toolchain provenance and classifications are in ZIPs.

Release passes 212 cases/691938 assertions and nine CLI groups. Extensions-OFF
ASan/UBSan passes 29 VM/plan cases/54305 assertions, fifteen VM CLI and six offset
groups. Fourteen profiling CLI groups and ten focused profile cases/46 assertions
pass. The generic checked/copy path remains the differential oracle.

## Decision and limits

Adopt this conservative stage for demonstrated heap-chain copy savings and
bounded measured costs. Ownership/backend alternatives under #202 and later
typed/compact execution stages under #207 remain open. Neither issue is closed
by these measurements. End-to-end speed includes startup and output; controls
are not subtracted. No compiler/hardware cause for the costs is asserted.

## full paired intervals

| Workload | Candidate / baseline, 95% interval |
| --- | --- |
| `startup` | 0.987 [0.925,1.090] |
| `loop_1000000` | 1.025 [1.014,1.028] |
| `calls_200000` | 1.016 [0.999,1.020] |
| `array_200000` | 1.023 [1.009,1.030] |
| `compare_high_1000d_x100000` | 1.012 [0.994,1.042] |
| `compare_low_1000d_x100000` | 1.015 [0.995,1.023] |
| `compare_equal_1000d_x100000` | 1.005 [0.986,1.020] |
| `fibonacci_1000_x100` | 0.994 [0.987,1.000] |
| `fibonacci_10000_x10` | 1.000 [0.996,1.008] |
| `factorial_1000_x50` | 0.989 [0.977,1.003] |
| `factorial_5000_x5` | 0.996 [0.993,1.005] |
| `multiply_100d_x100000` | 1.010 [0.988,1.017] |
| `multiply_1000d_x10000` | 0.998 [0.991,1.006] |
| `multiply_10000d_x1000` | 1.004 [0.996,1.010] |
| `divide_scale100_x100000` | 1.012 [1.004,1.015] |
| `divide_scale1000_x10000` | 0.992 [0.962,1.008] |
| `divide_scale5000_x5000` | 1.005 [0.990,1.022] |
| `sqrt_scale100_x5000` | 0.997 [0.990,1.001] |
| `sqrt_scale1000_x50` | 0.999 [0.992,1.006] |
| `sqrt_scale5000_x5` | 1.000 [0.995,1.004] |
| `multiply_dense_1000d_x5000` | 0.998 [0.980,1.014] |
| `multiply_dense_10000d_x500` | 0.994 [0.994,1.000] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.994 [0.989,1.031] |
| `multiply_sparse_10000d_x1000` | 1.002 [0.993,1.005] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.008 [0.971,1.099] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.983 [0.972,1.015] |
| `modulo_1000d_350d_x3000` | 1.001 [0.985,1.020] |
| `power_100d_e17_x1000` | 1.001 [0.983,1.009] |
| `power_20d_e32_x2000` | 1.000 [0.975,1.045] |
| `ibase_literal_flip_20000` | 1.001 [0.964,1.027] |
| `recursion_50_x1000` | 0.990 [0.975,1.014] |
| `sparse_array_200000` | 1.030 [1.018,1.036] |
| `frontend_definitions_2000` | 1.016 [0.994,1.036] |
| `decimal_input_4000d_x100` | 0.997 [0.986,1.004] |
| `decimal_output_200d_x1000` | 1.068 [0.940,1.172] |
| `streaming_statements_20000` | 0.996 [0.984,1.009] |
| `square_100d_x100000` | 1.011 [1.007,1.057] |
| `square_1000d_x10000` | 1.003 [0.998,1.008] |
| `square_10000d_x500` | 1.004 [0.998,1.006] |
| `literal_repeat_2000d_x2000` | 1.025 [0.969,1.079] |
| `binary_output_1000d_x300` | 0.980 [0.973,1.040] |
| `octal_output_1000d_x300` | 1.012 [0.983,1.022] |
| `binary_input_100000d_x3` | 1.002 [0.984,1.009] |
| `hex_input_100000d_x3` | 1.003 [0.995,1.011] |
| `temporary_chain_1000d_x50000` | 0.865 [0.861,0.868] |

## holdout paired intervals

| Workload | Candidate / baseline, 95% interval |
| --- | --- |
| `startup` | 1.089 [0.993,1.187] |
| `loop_875000` | 1.021 [1.005,1.028] |
| `calls_175000` | 1.023 [1.006,1.034] |
| `array_175000` | 1.012 [1.000,1.027] |
| `compare_high_1125d_x87500` | 1.016 [0.985,1.027] |
| `compare_low_1125d_x87500` | 1.009 [1.001,1.034] |
| `compare_equal_1125d_x87500` | 0.998 [0.988,1.010] |
| `fibonacci_1125_x88` | 0.991 [0.977,1.000] |
| `fibonacci_11250_x9` | 0.994 [0.978,1.009] |
| `factorial_1125_x44` | 0.987 [0.951,0.999] |
| `factorial_5625_x5` | 0.997 [0.990,1.000] |
| `multiply_112d_x87500` | 1.005 [0.993,1.008] |
| `multiply_1125d_x8750` | 0.986 [0.974,0.989] |
| `multiply_11250d_x875` | 0.997 [0.983,1.000] |
| `divide_scale112_x87500` | 1.022 [1.011,1.032] |
| `divide_scale1125_x8750` | 0.998 [0.992,1.012] |
| `divide_scale5625_x4375` | 0.997 [0.971,1.015] |
| `sqrt_scale112_x4375` | 0.999 [0.993,1.003] |
| `sqrt_scale1125_x44` | 1.018 [0.988,1.023] |
| `sqrt_scale5625_x5` | 1.000 [0.997,1.017] |
| `multiply_dense_1125d_x4375` | 0.999 [0.994,1.010] |
| `multiply_dense_11250d_x438` | 0.997 [0.991,1.014] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.981 [0.960,1.013] |
| `multiply_sparse_11250d_x875` | 0.996 [0.981,1.009] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.020 [0.959,1.082] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.986 [0.970,0.990] |
| `modulo_1125d_393d_x2625` | 1.031 [0.959,1.071] |
| `power_112d_e17_x875` | 1.001 [0.985,1.017] |
| `power_22d_e32_x1750` | 0.975 [0.965,1.028] |
| `ibase_literal_flip_17500` | 1.002 [0.977,1.062] |
| `recursion_56_x875` | 1.000 [0.984,1.018] |
| `sparse_array_175000` | 1.024 [1.012,1.034] |
| `frontend_definitions_1750` | 1.022 [1.013,1.033] |
| `decimal_input_4500d_x88` | 0.997 [0.986,1.005] |
| `decimal_output_225d_x875` | 1.000 [0.989,1.010] |
| `streaming_statements_17500` | 0.998 [0.990,1.024] |
| `square_112d_x87500` | 1.001 [0.994,1.011] |
| `square_1125d_x8750` | 1.005 [0.998,1.006] |
| `square_11250d_x438` | 1.001 [0.998,1.003] |
| `literal_repeat_2250d_x1750` | 1.043 [0.927,1.080] |
| `binary_output_1125d_x263` | 0.999 [0.985,1.039] |
| `octal_output_1125d_x263` | 1.009 [0.985,1.021] |
| `binary_input_112500d_x3` | 0.996 [0.991,1.012] |
| `hex_input_112500d_x3` | 1.002 [0.987,1.017] |
| `temporary_chain_1125d_x43750` | 0.875 [0.873,0.882] |
