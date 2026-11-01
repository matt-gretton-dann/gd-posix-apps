# Bounded execution-time literal cache — #205

Parent `58075b41a293a7d3f4cfbad308ff75997702e496` has identical production C++/CMake
as accepted main `a520f2a0db29858643dd0d24eb2470329d56ab56`. Candidate is
`f95455e053f5aef12a436ff2a1131eadedc780d6`. Both immutable recipes use fixed
126, Release -O3/-DNDEBUG, thin LTO, Apple Clang 21/libc++ header 220106 on the
recorded Apple M1/macOS 27.2 host. Manifests retain commands and binary hashes.
Pinned native Howard 7.0.3 is the competitive reference. No instrumented binary
is timed; no local build/test/profile overlaps the serial experiments.

All 111 quick, 1,110 full and 1,110 held-out invocations pass exact independent
output/status/stderr/deadline gates. Each full/held workload has one checked
warm-up and nine paired measured rounds, seeded interleaving and 2,000 bootstrap
resamples. Full seed 197200 and held-out dimensions/seed 197201 remain distinct.
The 37-case generator appends one repeated large literal without changing older
seeded operands. No aggregate or exclusions hide losing classes. The workstation
has no CPU/frequency/thermal isolation; these intervals do not promise behavior
on another host. Native run 33 remains a separately labeled experiment.

Correctness: test-first runtime-base/fraction/hex/redefinition/snapshot/operand
replacement tests pass, followed by populated-cache instruction copy/move tests.
All 158 Release bc cases (582,414 assertions), 18 extensions-disabled ASan/UBSan
VM/instruction cases (5,194 assertions), 15 sanitizer CLI cases, seven profile
checks and eight CLI groups pass. Function activations borrow persistent code;
instruction copies own independent memos. Serialize execution of one code vector.

## Division uncertainty

The full 900/450-digit division case is 1.070 [1.010, 1.089]; its held-out
1012/506-digit case is 1.147 [0.978, 1.151]. Both intervals cross the provisional
5% bound and remain inconclusive. A separately labeled bounded check, fixed
in advance at 27 paired rounds and three warm-ups, passes 90 checked invocations
per shape. It reports 1.041 [1.021, 1.058] full and 1.120 [1.003, 1.145] held-out.
These repeat intervals also cross the bound. The losing medians are retained;
this report does not claim division neutrality or a universal no-regression
result. The native artifacts have no confirmed regression, which does not
resolve every local shape. Follow-up arithmetic/compiler work under #197 still
needs to address the remaining competitive gaps.

## Counters and memory costs

All 74 separate full-corpus diagnostic runs pass and balance selected storage
at EOF. Counter binaries and exact source identities are recorded separately;
elapsed diagnostic times are not performance evidence. The size probe uses
each immutable recipe's actual include/flag context.

Instruction grows from 104 to 112 bytes on this 64-bit host; Number stays 40.
Each allocated memo requests 48 payload bytes, excluding allocator metadata.
Base misses replace the Number in place rather than allocating another memo.
These scoped requested-byte counters are not process malloc traffic or RSS.

| Workload | Conversions before / after | Memo allocations | Selected peak before / after |
| --- | --- | --- | --- |
| `loop_1000000` | 2000003 / 1 | 1 | 5040 / 5424 |
| `calls_200000` | 600003 / 1 | 1 | 7288 / 7704 |
| `literal_repeat_2000d_x2000` | 8002 / 2 | 2 | 9596 / 8536 |
| `ibase_literal_flip_20000` | 120003 / 40001 | 2 | 10552 / 11360 |
| `frontend_definitions_2000` | 1 / 0 | 0 | 5520 / 5872 |

Zero/one/single-digit integers need no memo allocation. General expression
folding is not performed. Bounded interning is not selected: measured hot small
constants already use direct integer construction, while the repeated large
literal has one persistent memo. The present corpus does not demonstrate a
material benefit from pooling duplicate large literal sites; a pool adds lookup,
ownership and invalidation costs. Revisit only with measured duplicate-site
workloads. This decision does not claim interning can never help.

## Native results and limits

All three verified native run-33 artifacts have no confirmed paired/A/A
regression. Loop ratios are Linux .831, macOS .848 and Windows .831. Repeated
large-literal ratios are .003/.006/.008. Windows call .943 [.937,1.190] remains
inconclusive; startup is uncalibrated. Exact artifacts, hashes, source revisions
and per-OS checked invocation counts remain beside the report.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.936 [0.932, 1.011] | 1.029 |
| `loop_1000000` | 0.807 [0.802, 0.828] | 1.066 |
| `calls_200000` | 0.928 [0.917, 0.935] | 1.610 |
| `array_200000` | 0.871 [0.863, 0.882] | 1.174 |
| `compare_high_1000d_x100000` | 0.881 [0.871, 0.897] | 1.148 |
| `compare_low_1000d_x100000` | 0.896 [0.888, 0.923] | 1.214 |
| `compare_equal_1000d_x100000` | 0.909 [0.897, 0.917] | 1.218 |
| `fibonacci_1000_x100` | 0.978 [0.968, 0.998] | 0.828 |
| `fibonacci_10000_x10` | 0.989 [0.977, 0.993] | 1.005 |
| `factorial_1000_x50` | 0.994 [0.989, 1.009] | 0.872 |
| `factorial_5000_x5` | 1.000 [0.997, 1.007] | 0.752 |
| `multiply_100d_x100000` | 0.926 [0.917, 0.932] | 0.680 |
| `multiply_1000d_x10000` | 1.001 [0.992, 1.003] | 2.043 |
| `multiply_10000d_x1000` | 1.001 [0.998, 1.004] | 6.186 |
| `divide_scale100_x100000` | 0.797 [0.790, 0.812] | 0.753 |
| `divide_scale1000_x10000` | 0.967 [0.940, 1.011] | 0.675 |
| `divide_scale5000_x5000` | 0.986 [0.973, 0.993] | 0.610 |
| `sqrt_scale100_x5000` | 1.000 [0.990, 1.004] | 0.602 |
| `sqrt_scale1000_x50` | 0.997 [0.985, 1.001] | 0.900 |
| `sqrt_scale5000_x5` | 0.995 [0.993, 1.011] | 0.852 |
| `multiply_dense_1000d_x5000` | 0.999 [0.970, 1.003] | 1.014 |
| `multiply_dense_10000d_x500` | 1.000 [0.994, 1.011] | 0.811 |
| `multiply_unbalanced_10000d_100d_x2000` | 0.925 [0.924, 0.986] | 0.718 |
| `multiply_sparse_10000d_x1000` | 0.998 [0.997, 1.002] | 2.617 |
| `divide_multilimb_900d_450d_s100_x1000` | 1.070 [1.010, 1.089] | 0.922 |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.998 [0.977, 1.011] | 0.941 |
| `modulo_1000d_350d_x3000` | 1.003 [0.993, 1.014] | 0.713 |
| `power_100d_e17_x1000` | 0.996 [0.963, 1.002] | 0.806 |
| `power_20d_e32_x2000` | 0.985 [0.941, 1.003] | 0.699 |
| `ibase_literal_flip_20000` | 0.981 [0.974, 1.010] | 1.185 |
| `recursion_50_x1000` | 0.967 [0.958, 0.992] | 1.937 |
| `sparse_array_200000` | 0.865 [0.856, 0.894] | 1.003 |
| `frontend_definitions_2000` | 0.996 [0.978, 1.024] | 3.445 |
| `decimal_input_4000d_x100` | 0.998 [0.994, 1.007] | 32.583 |
| `decimal_output_200d_x1000` | 0.988 [0.962, 0.999] | 2.071 |
| `streaming_statements_20000` | 0.999 [0.986, 1.019] | 2.968 |
| `literal_repeat_2000d_x2000` | 0.007 [0.006, 0.007] | 1.133 |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard (ratio of medians) |
| --- | --- | --- |
| `startup` | 0.976 [0.944, 1.029] | 1.020 |
| `loop_875000` | 0.817 [0.811, 0.825] | 1.064 |
| `calls_175000` | 0.928 [0.922, 0.935] | 1.626 |
| `array_175000` | 0.868 [0.860, 0.877] | 1.174 |
| `compare_high_1125d_x87500` | 0.865 [0.861, 0.884] | 1.112 |
| `compare_low_1125d_x87500` | 0.897 [0.821, 0.940] | 1.246 |
| `compare_equal_1125d_x87500` | 0.909 [0.902, 0.928] | 1.234 |
| `fibonacci_1125_x88` | 0.979 [0.957, 0.999] | 0.833 |
| `fibonacci_11250_x9` | 0.989 [0.988, 1.013] | 1.020 |
| `factorial_1125_x44` | 0.998 [0.993, 1.002] | 0.860 |
| `factorial_5625_x5` | 0.999 [0.996, 1.005] | 0.743 |
| `multiply_112d_x87500` | 0.946 [0.937, 0.954] | 0.690 |
| `multiply_1125d_x8750` | 0.999 [0.998, 1.005] | 2.302 |
| `multiply_11250d_x875` | 0.999 [0.960, 1.003] | 6.894 |
| `divide_scale112_x87500` | 0.885 [0.872, 0.900] | 0.732 |
| `divide_scale1125_x8750` | 0.985 [0.916, 1.034] | 0.587 |
| `divide_scale5625_x4375` | 0.995 [0.975, 1.004] | 0.488 |
| `sqrt_scale112_x4375` | 1.001 [0.994, 1.006] | 0.637 |
| `sqrt_scale1125_x44` | 0.997 [0.995, 1.001] | 0.888 |
| `sqrt_scale5625_x5` | 0.997 [0.974, 1.001] | 0.851 |
| `multiply_dense_1125d_x4375` | 0.998 [0.996, 1.002] | 1.048 |
| `multiply_dense_11250d_x438` | 1.002 [0.999, 1.004] | 0.859 |
| `multiply_unbalanced_11250d_112d_x1750` | 1.012 [1.006, 1.019] | 0.725 |
| `multiply_sparse_11250d_x875` | 0.997 [0.922, 1.002] | 2.860 |
| `divide_multilimb_1012d_506d_s112_x875` | 1.147 [0.978, 1.151] | 0.683 |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.998 [0.982, 1.006] | 1.253 |
| `modulo_1125d_393d_x2625` | 1.001 [0.985, 1.040] | 0.534 |
| `power_112d_e17_x875` | 1.000 [0.995, 1.024] | 0.842 |
| `power_22d_e32_x1750` | 0.993 [0.942, 1.037] | 0.737 |
| `ibase_literal_flip_17500` | 0.984 [0.942, 0.990] | 1.160 |
| `recursion_56_x875` | 0.966 [0.959, 0.983] | 1.916 |
| `sparse_array_175000` | 0.866 [0.861, 0.877] | 0.989 |
| `frontend_definitions_1750` | 1.002 [0.964, 1.021] | 3.290 |
| `decimal_input_4500d_x88` | 0.997 [0.996, 1.001] | 37.011 |
| `decimal_output_225d_x875` | 0.993 [0.985, 1.019] | 2.105 |
| `streaming_statements_17500` | 0.984 [0.978, 1.004] | 2.929 |
| `literal_repeat_2250d_x1750` | 0.006 [0.005, 0.007] | 1.109 |
