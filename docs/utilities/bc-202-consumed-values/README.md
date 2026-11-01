# Consumed parameter nodes and terminal returns — #202

Move values only where their source lifetime ends: a parameter node immediately
before erasure, or an owned numeric producer slot at a terminal return. Variable
and array locations are never moved from. Recursive caller snapshots, array COW,
local restoration and fractional scale/sign have exact automated output checks.
This does not infer general arithmetic liveness across backward edges.

Parent is `73619a3e5dbc3dbddbca929bdac0a212b77b6772`; candidate is
`2ce200c69e3fbd3b148c922e1b3f9281dc74badb`. Immutable Release recipes use fixed
threshold 126, Apple Clang 21, libc++ header 220106 and thin LTO on Apple M1,
macOS 27.2. Compiler commands, hashes and corpus identities are in the manifests.

## Checked process measurements

Each full/held-out experiment checks all 36 workloads across four implementations:
parent, this candidate, the separate two-state inline-storage experiment and
pinned native Howard 7.0.3. There are 1,440 checked invocations per experiment
(9 paired measured rounds plus a warm-up per case/implementation). Raw rows and
summaries retain every implementation, including losing observations. This report
extracts only `gd-consume / gd-parent`; it does not attribute inline-storage gains
to consumption. All builds, tests and diagnostics finished before timing began.
Instrumented binaries do not enter timings. No CPU/frequency/thermal isolation
is claimed; intervals are paired bootstrap estimates, not cross-host guarantees.
Full seed is 197200; held-out shapes/seed are 197201, with 2,000 bootstrap resamples.

The call workload loses 400,000 owned-digit copies (4,400,005 to 4,000,005),
replaced by moves. Selected allocation calls are unchanged at 4,768,409. The loop
and dense array counters are unchanged. This is an ownership-operation reduction,
not an allocation or broad runtime win. All 72 parent/candidate profiles balance
selected allocations/deallocations and have zero selected live bytes at EOF.
The parent diagnostic binary's source e7c1a83 has the same Git tree as 73619a3;
that identity is established by the earlier #211/current-storage evidence.

The full call timing is 0.988 [0.980, 1.000], while most unrelated classes are
neutral. Retain the complete class table rather than an aggregate performance
claim. Native paired CI reports are checked separately before merge.

| Corpus | Workload | Candidate / parent [95%] |
| --- | --- | --- |
| full | `startup` | 1.002 [0.938, 1.029] |
| full | `loop_1000000` | 0.990 [0.984, 0.994] |
| full | `calls_200000` | 0.988 [0.980, 1.000] |
| full | `array_200000` | 0.980 [0.957, 1.024] |
| full | `compare_high_1000d_x100000` | 0.985 [0.977, 0.999] |
| full | `compare_low_1000d_x100000` | 0.992 [0.973, 1.008] |
| full | `compare_equal_1000d_x100000` | 0.987 [0.979, 1.005] |
| full | `fibonacci_1000_x100` | 0.996 [0.993, 1.014] |
| full | `fibonacci_10000_x10` | 0.995 [0.985, 1.003] |
| full | `factorial_1000_x50` | 0.997 [0.992, 0.998] |
| full | `factorial_5000_x5` | 0.999 [0.992, 1.003] |
| full | `multiply_100d_x100000` | 0.994 [0.987, 1.019] |
| full | `multiply_1000d_x10000` | 0.999 [0.999, 1.006] |
| full | `multiply_10000d_x1000` | 1.000 [0.997, 1.006] |
| full | `divide_scale100_x100000` | 0.994 [0.992, 1.008] |
| full | `divide_scale1000_x10000` | 0.995 [0.979, 1.004] |
| full | `divide_scale5000_x5000` | 0.993 [0.948, 1.003] |
| full | `sqrt_scale100_x5000` | 1.001 [0.996, 1.017] |
| full | `sqrt_scale1000_x50` | 1.002 [0.998, 1.007] |
| full | `sqrt_scale5000_x5` | 1.003 [0.998, 1.006] |
| full | `multiply_dense_1000d_x5000` | 1.001 [0.994, 1.020] |
| full | `multiply_dense_10000d_x500` | 0.996 [0.992, 1.002] |
| full | `multiply_unbalanced_10000d_100d_x2000` | 0.992 [0.990, 1.000] |
| full | `multiply_sparse_10000d_x1000` | 1.001 [0.999, 1.003] |
| full | `divide_multilimb_900d_450d_s100_x1000` | 0.995 [0.975, 1.054] |
| full | `divide_multilimb_1000d_900d_s1000_x500` | 1.006 [0.985, 1.036] |
| full | `modulo_1000d_350d_x3000` | 0.998 [0.986, 1.028] |
| full | `power_100d_e17_x1000` | 0.997 [0.989, 1.000] |
| full | `power_20d_e32_x2000` | 1.004 [0.998, 1.012] |
| full | `ibase_literal_flip_20000` | 0.978 [0.959, 0.997] |
| full | `recursion_50_x1000` | 1.000 [0.992, 1.006] |
| full | `sparse_array_200000` | 0.990 [0.976, 1.005] |
| full | `frontend_definitions_2000` | 1.001 [0.988, 1.022] |
| full | `decimal_input_4000d_x100` | 0.998 [0.996, 1.009] |
| full | `decimal_output_200d_x1000` | 0.990 [0.945, 1.112] |
| full | `streaming_statements_20000` | 0.999 [0.992, 1.020] |
| holdout | `startup` | 0.993 [0.971, 1.228] |
| holdout | `loop_875000` | 0.989 [0.987, 1.021] |
| holdout | `calls_175000` | 0.987 [0.980, 0.992] |
| holdout | `array_175000` | 0.988 [0.946, 1.028] |
| holdout | `compare_high_1125d_x87500` | 0.988 [0.964, 0.997] |
| holdout | `compare_low_1125d_x87500` | 0.990 [0.966, 1.000] |
| holdout | `compare_equal_1125d_x87500` | 0.986 [0.955, 0.998] |
| holdout | `fibonacci_1125_x88` | 0.987 [0.979, 0.990] |
| holdout | `fibonacci_11250_x9` | 0.995 [0.987, 0.999] |
| holdout | `factorial_1125_x44` | 0.994 [0.987, 1.004] |
| holdout | `factorial_5625_x5` | 1.000 [0.991, 1.003] |
| holdout | `multiply_112d_x87500` | 0.994 [0.990, 1.002] |
| holdout | `multiply_1125d_x8750` | 1.000 [0.999, 1.012] |
| holdout | `multiply_11250d_x875` | 1.001 [0.988, 1.007] |
| holdout | `divide_scale112_x87500` | 0.992 [0.981, 0.994] |
| holdout | `divide_scale1125_x8750` | 1.001 [0.991, 1.032] |
| holdout | `divide_scale5625_x4375` | 0.999 [0.997, 1.007] |
| holdout | `sqrt_scale112_x4375` | 1.000 [0.994, 1.005] |
| holdout | `sqrt_scale1125_x44` | 1.000 [0.993, 1.004] |
| holdout | `sqrt_scale5625_x5` | 1.004 [0.999, 1.016] |
| holdout | `multiply_dense_1125d_x4375` | 0.999 [0.995, 1.016] |
| holdout | `multiply_dense_11250d_x438` | 1.000 [0.995, 1.005] |
| holdout | `multiply_unbalanced_11250d_112d_x1750` | 1.002 [0.998, 1.036] |
| holdout | `multiply_sparse_11250d_x875` | 1.000 [0.952, 1.007] |
| holdout | `divide_multilimb_1012d_506d_s112_x875` | 1.050 [0.970, 1.173] |
| holdout | `divide_multilimb_1125d_1012d_s1125_x438` | 1.003 [0.995, 1.043] |
| holdout | `modulo_1125d_393d_x2625` | 1.041 [1.015, 1.105] |
| holdout | `power_112d_e17_x875` | 0.999 [0.993, 1.009] |
| holdout | `power_22d_e32_x1750` | 1.013 [0.995, 1.041] |
| holdout | `ibase_literal_flip_17500` | 0.980 [0.963, 0.996] |
| holdout | `recursion_56_x875` | 1.000 [0.994, 1.011] |
| holdout | `sparse_array_175000` | 0.989 [0.959, 1.001] |
| holdout | `frontend_definitions_1750` | 1.006 [0.979, 1.013] |
| holdout | `decimal_input_4500d_x88` | 0.999 [0.993, 1.011] |
| holdout | `decimal_output_225d_x875` | 0.996 [0.978, 1.006] |
| holdout | `streaming_statements_17500` | 1.014 [0.997, 1.024] |

#202 remains open: general liveness, result-capacity reuse and ownership/arena
evaluation are separate work. POSIX math workload coverage remains #217.
