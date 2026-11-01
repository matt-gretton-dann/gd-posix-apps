# Portable blocked schoolbook multiplication

Candidate `565564dba7d781613c5046d4898a0efdf6a2832b` versus accepted source
`0868b4acd36d91a39a1d07c1aafb0808bda519cc`. Native PR base is
`9eed920f181f3445db5a4017d48f3d6c0e4ed249`; its production arithmetic/VM
sources are identical to 0868b4a (intervening changes are benchmark selection,
reversed-operand tests and rejected-experiment documentation). Every identity
remains explicit rather than relabeling a previously built executable.

All 120 quick, 1200 full and 1200 held-out invocations pass exact output, status,
stderr and deadline checks. Nine seeded paired measurement rounds follow one
checked warm-up per implementation/case; 2000 paired bootstrap resamples.
Apple M1/macOS 27.2, Apple Clang 21/libc++ header 220106, Release thin LTO,
extensions ON and fixed split point 126. No local builds/tests/profiles overlap
timing. These are end-to-end process measurements, not pure kernel intervals.

Full multiplication ratios range from 0.222 to 0.582; held-out 0.229 to 0.544.
All square shapes remain close to baseline. Local multi-limb division remains
inconclusive: full 1.053 [1.016,1.090], held-out 1.150 [0.978,1.207]. Do not
claim neutral division or hide the worst class. No frequency/thermal isolation
or cross-platform precision guarantee follows from the shared local host.

Native run 41276001021 passes 1450 checked invocations per OS on Linux, macOS
and Windows, including independent A/A builds, with no confirmed regressions.
Large dense multiplication ratios are 0.472 Linux, 0.318 macOS and 0.489
Windows. Division is Linux pass and macOS/Windows inconclusive. Verified raw
ZIPs, their SHA-256 digests, confirmations and losing classes remain archived.

All 80 separate parent/candidate diagnostic invocations pass exact oracles and
balance selected allocations at EOF. Counter binaries retain their actual
source identities, including parent e069301 (production-equivalent to 0868b4a).
Counter data are not elapsed timings or process RSS. Dedicated square dispatch
and one-limb paths remain ahead of the candidate, with scalar fallback for
small or insufficient-headroom shapes. No ISA/vectorization cause is claimed.

This accepts only the bounded base-case experiment after green final checks.
Robust crossover/default selection remains #203 work; alternative algorithms
and backends remain #204 work. The overall #197 competitive target is not
claimed achieved.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard (medians) |
| --- | --- | --- |
| `startup` | 0.960 [0.926, 1.123] | 1.026 |
| `loop_1000000` | 0.997 [0.994, 1.002] | 1.059 |
| `calls_200000` | 0.996 [0.993, 0.998] | 1.612 |
| `array_200000` | 0.996 [0.994, 1.000] | 1.164 |
| `compare_high_1000d_x100000` | 0.998 [0.990, 1.023] | 1.125 |
| `compare_low_1000d_x100000` | 1.005 [0.991, 1.018] | 1.251 |
| `compare_equal_1000d_x100000` | 1.004 [0.998, 1.009] | 1.244 |
| `fibonacci_1000_x100` | 0.994 [0.983, 1.005] | 0.825 |
| `fibonacci_10000_x10` | 1.002 [0.994, 1.008] | 1.005 |
| `factorial_1000_x50` | 0.998 [0.991, 1.005] | 0.868 |
| `factorial_5000_x5` | 0.999 [0.995, 1.039] | 0.746 |
| `multiply_100d_x100000` | 0.582 [0.580, 0.585] | 0.400 |
| `multiply_1000d_x10000` | 0.239 [0.238, 0.243] | 0.492 |
| `multiply_10000d_x1000` | 0.311 [0.310, 0.322] | 1.937 |
| `divide_scale100_x100000` | 1.001 [0.997, 1.004] | 0.751 |
| `divide_scale1000_x10000` | 0.997 [0.989, 1.029] | 0.660 |
| `divide_scale5000_x5000` | 1.001 [0.996, 1.020] | 0.614 |
| `sqrt_scale100_x5000` | 1.008 [0.999, 1.017] | 0.605 |
| `sqrt_scale1000_x50` | 0.999 [0.979, 1.019] | 0.905 |
| `sqrt_scale5000_x5` | 0.998 [0.997, 1.004] | 0.853 |
| `multiply_dense_1000d_x5000` | 0.222 [0.220, 0.227] | 0.225 |
| `multiply_dense_10000d_x500` | 0.319 [0.317, 0.320] | 0.260 |
| `multiply_unbalanced_10000d_100d_x2000` | 0.365 [0.361, 0.368] | 0.284 |
| `multiply_sparse_10000d_x1000` | 0.398 [0.396, 0.402] | 1.048 |
| `divide_multilimb_900d_450d_s100_x1000` | 1.052 [1.016, 1.090] | 0.919 |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.998 [0.990, 1.025] | 0.949 |
| `modulo_1000d_350d_x3000` | 0.762 [0.752, 0.773] | 0.542 |
| `power_100d_e17_x1000` | 0.791 [0.780, 0.796] | 0.400 |
| `power_20d_e32_x2000` | 0.997 [0.983, 1.033] | 0.504 |
| `ibase_literal_flip_20000` | 1.004 [0.991, 1.030] | 1.169 |
| `recursion_50_x1000` | 0.995 [0.960, 1.019] | 1.926 |
| `sparse_array_200000` | 0.992 [0.987, 1.002] | 0.989 |
| `frontend_definitions_2000` | 1.016 [1.003, 1.046] | 3.552 |
| `decimal_input_4000d_x100` | 0.993 [0.983, 0.999] | 32.986 |
| `decimal_output_200d_x1000` | 1.001 [0.993, 1.027] | 2.151 |
| `streaming_statements_20000` | 1.001 [0.982, 1.006] | 3.037 |
| `square_100d_x100000` | 0.999 [0.989, 1.002] | 0.532 |
| `square_1000d_x10000` | 1.002 [0.997, 1.005] | 0.486 |
| `square_10000d_x500` | 1.002 [0.999, 1.014] | 0.418 |
| `literal_repeat_2000d_x2000` | 1.000 [0.960, 1.040] | 1.026 |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard (medians) |
| --- | --- | --- |
| `startup` | 0.983 [0.891, 1.184] | 1.024 |
| `loop_875000` | 0.998 [0.994, 1.003] | 1.069 |
| `calls_175000` | 0.995 [0.991, 1.003] | 1.604 |
| `array_175000` | 1.002 [0.998, 1.004] | 1.170 |
| `compare_high_1125d_x87500` | 0.988 [0.987, 1.004] | 1.126 |
| `compare_low_1125d_x87500` | 1.001 [0.984, 1.011] | 1.251 |
| `compare_equal_1125d_x87500` | 1.005 [0.984, 1.011] | 1.241 |
| `fibonacci_1125_x88` | 0.998 [0.992, 1.007] | 0.829 |
| `fibonacci_11250_x9` | 1.003 [0.996, 1.011] | 1.021 |
| `factorial_1125_x44` | 0.995 [0.985, 1.009] | 0.854 |
| `factorial_5625_x5` | 1.000 [0.998, 1.005] | 0.740 |
| `multiply_112d_x87500` | 0.544 [0.541, 0.551] | 0.379 |
| `multiply_1125d_x8750` | 0.236 [0.234, 0.237] | 0.548 |
| `multiply_11250d_x875` | 0.288 [0.287, 0.289] | 1.992 |
| `divide_scale112_x87500` | 0.995 [0.988, 0.999] | 0.734 |
| `divide_scale1125_x8750` | 0.998 [0.977, 1.006] | 0.580 |
| `divide_scale5625_x4375` | 0.999 [0.994, 1.012] | 0.487 |
| `sqrt_scale112_x4375` | 1.008 [0.997, 1.013] | 0.638 |
| `sqrt_scale1125_x44` | 1.001 [0.991, 1.005] | 0.887 |
| `sqrt_scale5625_x5` | 1.001 [0.995, 1.009] | 0.852 |
| `multiply_dense_1125d_x4375` | 0.229 [0.228, 0.232] | 0.239 |
| `multiply_dense_11250d_x438` | 0.295 [0.294, 0.297] | 0.254 |
| `multiply_unbalanced_11250d_112d_x1750` | 0.403 [0.401, 0.404] | 0.293 |
| `multiply_sparse_11250d_x875` | 0.378 [0.377, 0.381] | 1.082 |
| `divide_multilimb_1012d_506d_s112_x875` | 1.150 [0.978, 1.207] | 0.721 |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.999 [0.986, 1.017] | 1.248 |
| `modulo_1125d_393d_x2625` | 0.725 [0.720, 0.757] | 0.393 |
| `power_112d_e17_x875` | 0.796 [0.786, 0.799] | 0.411 |
| `power_22d_e32_x1750` | 1.003 [0.986, 1.015] | 0.507 |
| `ibase_literal_flip_17500` | 1.005 [0.973, 1.039] | 1.169 |
| `recursion_56_x875` | 0.996 [0.968, 1.023] | 1.911 |
| `sparse_array_175000` | 0.996 [0.991, 1.002] | 0.990 |
| `frontend_definitions_1750` | 1.015 [1.008, 1.023] | 3.341 |
| `decimal_input_4500d_x88` | 0.993 [0.992, 0.997] | 36.748 |
| `decimal_output_225d_x875` | 0.986 [0.985, 1.033] | 2.117 |
| `streaming_statements_17500` | 1.005 [0.988, 1.020] | 3.051 |
| `square_112d_x87500` | 1.001 [0.993, 1.006] | 0.538 |
| `square_1125d_x8750` | 0.977 [0.971, 0.978] | 0.509 |
| `square_11250d_x438` | 1.003 [1.000, 1.006] | 0.433 |
| `literal_repeat_2250d_x1750` | 0.972 [0.870, 0.982] | 0.960 |
