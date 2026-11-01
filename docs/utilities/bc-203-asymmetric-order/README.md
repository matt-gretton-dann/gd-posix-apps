# Shorter-row schoolbook order — rejected #203 experiment

**Do not adopt this ordering.** The intended asymmetric workload is slower in
both local presets and has confirmed native Linux/Windows regressions. Correct
values and fewer potential row tails do not establish faster execution. Restore
the accepted row order while retaining the reversed-operand oracle coverage.

Parent `a5c1cb1ad4051616afdce115b02ecc0247ad62f7`; candidate
`c73fb2a6ee86d1a0720c8e67ee4b8c620baede43`. Immutable Release recipes use identical
Apple Clang21/libc++ header220106, -O3/-DNDEBUG, thin LTO, extensions ON and fixed
126 on the recorded Apple M1/macOS27.2 host. Manifests retain exact commands and
binary hashes. Native pinned Howard7.0.3 is the competitive reference.

All120 quick,1200 full and1200 held-out checks pass independent exact output,
status, stderr and deadline gates. Each full/held case has one checked warm-up
and nine paired rounds, seeded order and2000 bootstrap resamples. Held-out
dimensions/seed197201 remain separate from full197200. No local builds/tests/
profiles overlap timing. This shared workstation has no frequency/thermal
isolation. All40 classes, including losses and inconclusive cases, remain below.

Full unbalanced product ratio1.114 [1.108,1.123]; held-out1.111 [1.108,1.118].
Full modulo is1.067 [1.053,1.076]. Division and additional modulo observations
remain slower/inconclusive in their full tables. No aggregate hides them.

Native run44 uses exact parent/candidate identities. Each OS passes1130 checked
invocations including bounded confirmations. Linux unbalanced multiplication is
1.226 [1.225,1.228] and Windows1.133 [1.099,1.140], both confirmed regressions.
macOS1.076 [1.014,1.225] remains inconclusive. Raw verified ZIPs/hashes and all
confirmation rows are preserved. The change is rejected across the portable
default rather than selecting an unsupported platform-specific exception.

All80 separate parent/candidate full-corpus diagnostic runs pass exact oracles
and balance selected storage at EOF. They are not timed; no hardware cause is
established. Dependency chains, locality and code generation are hypotheses,
not measured explanations for these results. Planned blocked kernels and robust
crossover/default selection remain separate #203 work.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.992 [0.907,1.152] | 1.033 |
| `loop_1000000` | 0.999 [0.995,1.001] | 1.066 |
| `calls_200000` | 1.000 [0.989,1.002] | 1.612 |
| `array_200000` | 1.001 [0.997,1.008] | 1.171 |
| `compare_high_1000d_x100000` | 1.001 [1.000,1.015] | 1.136 |
| `compare_low_1000d_x100000` | 1.000 [0.996,1.012] | 1.249 |
| `compare_equal_1000d_x100000` | 0.995 [0.987,1.010] | 1.234 |
| `fibonacci_1000_x100` | 1.003 [0.996,1.038] | 0.830 |
| `fibonacci_10000_x10` | 1.001 [0.998,1.014] | 1.002 |
| `factorial_1000_x50` | 1.000 [0.997,1.025] | 0.876 |
| `factorial_5000_x5` | 1.000 [0.996,1.008] | 0.751 |
| `multiply_100d_x100000` | 1.029 [1.018,1.033] | 0.707 |
| `multiply_1000d_x10000` | 0.998 [0.996,1.001] | 2.044 |
| `multiply_10000d_x1000` | 0.987 [0.986,0.994] | 6.154 |
| `divide_scale100_x100000` | 0.998 [0.988,1.005] | 0.747 |
| `divide_scale1000_x10000` | 0.999 [0.996,1.013] | 0.673 |
| `divide_scale5000_x5000` | 0.999 [0.977,1.034] | 0.618 |
| `sqrt_scale100_x5000` | 1.008 [1.006,1.020] | 0.607 |
| `sqrt_scale1000_x50` | 0.996 [0.985,1.007] | 0.903 |
| `sqrt_scale5000_x5` | 1.001 [0.996,1.004] | 0.854 |
| `multiply_dense_1000d_x5000` | 1.004 [1.000,1.008] | 1.015 |
| `multiply_dense_10000d_x500` | 0.994 [0.990,0.998] | 0.810 |
| `multiply_unbalanced_10000d_100d_x2000` | 1.114 [1.108,1.123] | 0.805 |
| `multiply_sparse_10000d_x1000` | 0.997 [0.995,1.004] | 2.625 |
| `divide_multilimb_900d_450d_s100_x1000` | 1.063 [1.028,1.110] | 0.926 |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.010 [0.995,1.038] | 0.965 |
| `modulo_1000d_350d_x3000` | 1.067 [1.053,1.076] | 0.759 |
| `power_100d_e17_x1000` | 1.001 [0.994,1.007] | 0.505 |
| `power_20d_e32_x2000` | 1.002 [0.997,1.009] | 0.522 |
| `ibase_literal_flip_20000` | 1.009 [0.981,1.042] | 1.171 |
| `recursion_50_x1000` | 0.999 [0.979,1.008] | 1.932 |
| `sparse_array_200000` | 1.001 [0.997,1.005] | 0.996 |
| `frontend_definitions_2000` | 0.994 [0.983,1.008] | 3.445 |
| `decimal_input_4000d_x100` | 0.993 [0.988,0.998] | 33.844 |
| `decimal_output_200d_x1000` | 0.997 [0.986,1.040] | 2.139 |
| `streaming_statements_20000` | 0.994 [0.987,1.003] | 3.002 |
| `square_100d_x100000` | 0.994 [0.990,0.995] | 0.530 |
| `square_1000d_x10000` | 0.990 [0.985,0.994] | 0.481 |
| `square_10000d_x500` | 0.997 [0.995,1.000] | 0.416 |
| `literal_repeat_2000d_x2000` | 0.991 [0.939,1.018] | 0.979 |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.967 [0.859,1.110] | 1.029 |
| `loop_875000` | 1.000 [0.998,1.003] | 1.056 |
| `calls_175000` | 0.996 [0.994,0.998] | 1.605 |
| `array_175000` | 1.003 [0.998,1.009] | 1.168 |
| `compare_high_1125d_x87500` | 0.993 [0.987,1.006] | 1.131 |
| `compare_low_1125d_x87500` | 0.989 [0.970,1.003] | 1.243 |
| `compare_equal_1125d_x87500` | 0.999 [0.975,1.014] | 1.245 |
| `fibonacci_1125_x88` | 0.999 [0.989,1.049] | 0.831 |
| `fibonacci_11250_x9` | 1.000 [0.997,1.013] | 1.019 |
| `factorial_1125_x44` | 1.000 [0.996,1.016] | 0.862 |
| `factorial_5625_x5` | 0.996 [0.995,1.004] | 0.745 |
| `multiply_112d_x87500` | 0.965 [0.954,0.969] | 0.672 |
| `multiply_1125d_x8750` | 0.998 [0.997,0.999] | 2.298 |
| `multiply_11250d_x875` | 0.999 [0.995,1.006] | 6.928 |
| `divide_scale112_x87500` | 0.998 [0.987,0.999] | 0.734 |
| `divide_scale1125_x8750` | 0.999 [0.986,1.012] | 0.577 |
| `divide_scale5625_x4375` | 1.012 [0.991,1.036] | 0.488 |
| `sqrt_scale112_x4375` | 1.004 [1.004,1.014] | 0.638 |
| `sqrt_scale1125_x44` | 0.997 [0.989,0.999] | 0.887 |
| `sqrt_scale5625_x5` | 1.002 [0.998,1.017] | 0.852 |
| `multiply_dense_1125d_x4375` | 1.001 [0.998,1.002] | 1.046 |
| `multiply_dense_11250d_x438` | 1.005 [1.002,1.019] | 0.864 |
| `multiply_unbalanced_11250d_112d_x1750` | 1.111 [1.108,1.118] | 0.807 |
| `multiply_sparse_11250d_x875` | 0.999 [0.996,1.001] | 2.863 |
| `divide_multilimb_1012d_506d_s112_x875` | 1.123 [1.018,1.213] | 0.711 |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.010 [0.998,1.052] | 1.256 |
| `modulo_1125d_393d_x2625` | 1.055 [1.044,1.150] | 0.569 |
| `power_112d_e17_x875` | 0.998 [0.955,1.006] | 0.520 |
| `power_22d_e32_x1750` | 0.998 [0.991,1.023] | 0.509 |
| `ibase_literal_flip_17500` | 1.001 [0.967,1.057] | 1.160 |
| `recursion_56_x875` | 0.998 [0.991,1.025] | 1.929 |
| `sparse_array_175000` | 0.996 [0.988,1.009] | 0.986 |
| `frontend_definitions_1750` | 0.996 [0.988,1.010] | 3.321 |
| `decimal_input_4500d_x88` | 0.992 [0.990,1.000] | 36.933 |
| `decimal_output_225d_x875` | 0.989 [0.962,1.002] | 2.105 |
| `streaming_statements_17500` | 0.999 [0.996,1.008] | 2.971 |
| `square_112d_x87500` | 0.999 [0.992,1.005] | 0.539 |
| `square_1125d_x8750` | 0.984 [0.950,0.987] | 0.514 |
| `square_11250d_x438` | 1.000 [0.975,1.002] | 0.432 |
| `literal_repeat_2250d_x1750` | 0.994 [0.916,1.119] | 1.059 |
