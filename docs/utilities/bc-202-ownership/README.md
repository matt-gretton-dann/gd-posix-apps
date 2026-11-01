# Numeric ownership policy comparison — #202

Refs #197. Retain the current three-inline-limb/COW hybrid. Neither alternative
below is adopted. These are actual full-VM source experiments, not isolated
reference-count or allocator microbenchmarks.

| Policy | Exact source |
| --- | --- |
| Production hybrid | `7974cb3d8b16c98441f9dc960f6c91f52736b06d` |
| Three inline limbs, unique/deep-copy heap | `1c5a294f2c7c8720521ad89ff74939221fb93348` |
| Zero inline capacity, shared heap COW control | `57d88bb97d2d2c99cf18e4850cd692ba26ea79d2` |

The zero-inline control uses current algorithms and verified dispatch, not an
old executable relabeled as current. Empty values remain allocation-free; one
unused local slot supplies valid empty-range pointers. Nonempty coefficients
use the heap. The unique prototype keeps three inline limbs, eagerly clones
heap vectors, and removes false noexcept promises from copying. Moves remain
nonthrowing. Only representation-specific shared-owner assertions are adapted;
numeric values, snapshots, scale, sign, arithmetic and VM algorithms are unchanged.
Exact patches are retained here, and the source branches are pushed.

## Correctness and matched timing

Unique ownership passes 212 Release cases/691942 assertions, CLI/VM/independent
arithmetic groups, and 170 number/VM cases/507710 assertions under extensions-OFF
ASan/UBSan plus the VM CLI checks. The heap control passes CLI, VM and independent
arithmetic groups. All three frozen binaries pass 45 quick workload oracles.
The heap control does not claim the hybrid-specific storage-layout tests or an
ASan run; neither rejected prototype has native cross-platform acceptance.

Apple M1, macOS 27.2, Apple Clang 21, libc++ 220106, Release thin LTO, split 126,
extensions ON. Full and held-out datasets each check 1350 invocations: 45 cases,
three policies, one warmup and nine shuffled paired samples. All 2700 pass exact
output/status/stderr/deadline checks. Seeds 197229/197230 schedule paired rounds;
197200/197201 generate full/held-out operands. Intervals use 2000 bootstrap
resamples. No builds, tests, profiles or RSS work overlap timings. Scheduling,
frequency and thermals remain uncontrolled. Every class is retained below;
startup is separate and controls are not subtracted from process wall time.

Unique ownership has CI95 wholly above 1.05 in 11 classes in each dataset.
Short/long Fibonacci ratios are 1.767/1.630 full and 1.801/1.603 held-out;
the temporary chain is 1.449/1.398. Scalar loop/calls improve about 2%, which
does not offset those losses. Zero-inline COW has 19/17 such losing classes:
loop 1.749/1.727, calls 1.484/1.464 and dense array 1.595/1.602. This supports
retaining cheap immutable/snapshot sharing and inline scalar storage on this
host. It does not prove every ownership alternative loses on every machine.

## Separate process memory and layout

All 405 separate fresh-process memory executions pass exact output and process
checks: three samples per full case/policy, after timing. Raw maximum RSS uses
the local byte-valued getrusage convention, whose reference is retained. These
are process peaks including loader, frontend, values, code and output, not
isolated coefficient or allocator payload. Three samples are not portable
peak bounds or confidence intervals.

| Full workload | Hybrid RSS bytes | Unique RSS bytes | Heap COW RSS bytes |
| --- | ---: | ---: | ---: |
| `loop_1000000` | 1966080 | 2015232 | 2048000 |
| `calls_200000` | 1998848 | 2031616 | 2048000 |
| `array_200000` | 24281088 | 20643840 | 33538048 |
| `fibonacci_1000_x100` | 2244608 | 2310144 | 2424832 |
| `fibonacci_10000_x10` | 2736128 | 2457600 | 2981888 |
| `temporary_chain_1000d_x50000` | 2064384 | 2113536 | 2064384 |

Unique ownership has a real memory tradeoff: dense-array process RSS falls
from 24281088 to 20643840 bytes here. Zero-inline COW instead rises to 33538048.
Neither is a universal memory reduction. Actual compiler probes show native
Number sizes 40/32/32 bytes and storage sizes 32/24/24; Instruction stays 112.
All three Mach-O __TEXT segments are 311296 bytes; executable files are
508048/506752/508048 bytes. The commands and binary hashes are retained. The
unique owner bypasses shared-factory accounting, so no comparison of selected
owner-byte or clone totals is used as an allocator or RSS claim.

## Lifetime and scratch policy

Retain the bounded per-operation multiplication workspace accepted in #203:
its spans are borrowed only while their owner lives, and no scratch view escapes
or crosses a recursive activation. Reusable VM result/frame buffers from #208
retain capacity under their existing owner; conservative transfers from #342
reuse a working numeric coefficient only after its last-use proof. These do not
license mutation of live snapshots or literal-cache owners.

Do not introduce a VM-wide monotonic numeric arena. Globals, arrays, registered
functions and decoded constants survive input items; returned values and dynamic
bindings can survive the activation that created them. Resetting at either
boundary would invalidate reachable values. Keeping the arena until VM teardown
instead retains allocations across an arbitrarily long stream. A future recycling
arena would need independently proved escape/promotion and reclamation rules;
none is asserted here. Keep the portable ordinary allocator/COW fallback rather
than adding unproved cross-frame lifetimes. This is a lifetime-based rejection,
not a measured comparison against an implemented arena. Cold diagnostic separation
is evaluated separately; this report completes only the ownership comparison.

## full paired intervals

| Workload | Unique / hybrid, CI95 | Heap COW / hybrid, CI95 |
| --- | --- | --- |
| `startup` | 0.946 [0.876,1.077] | 0.952 [0.932,1.034] |
| `loop_1000000` | 0.979 [0.961,0.997] | 1.749 [1.739,1.761] |
| `calls_200000` | 0.985 [0.965,1.000] | 1.484 [1.453,1.504] |
| `array_200000` | 1.001 [0.985,1.024] | 1.596 [1.563,1.602] |
| `compare_high_1000d_x100000` | 1.306 [1.260,1.315] | 1.755 [1.721,1.781] |
| `compare_low_1000d_x100000` | 1.215 [1.187,1.219] | 1.558 [1.540,1.559] |
| `compare_equal_1000d_x100000` | 1.202 [1.176,1.277] | 1.555 [1.536,1.827] |
| `fibonacci_1000_x100` | 1.767 [1.759,1.798] | 1.401 [1.392,1.410] |
| `fibonacci_10000_x10` | 1.630 [1.624,1.640] | 1.214 [1.210,1.230] |
| `factorial_1000_x50` | 1.151 [1.133,1.162] | 1.202 [1.188,1.213] |
| `factorial_5000_x5` | 1.086 [1.077,1.107] | 1.056 [1.050,1.097] |
| `multiply_100d_x100000` | 1.318 [1.304,1.322] | 1.312 [1.304,1.330] |
| `multiply_1000d_x10000` | 1.012 [1.005,1.023] | 1.025 [1.018,1.042] |
| `multiply_10000d_x1000` | 1.005 [0.990,1.006] | 1.007 [0.994,1.017] |
| `divide_scale100_x100000` | 1.073 [1.055,1.088] | 1.667 [1.656,1.680] |
| `divide_scale1000_x10000` | 1.026 [1.018,1.033] | 1.168 [1.143,1.183] |
| `divide_scale5000_x5000` | 1.007 [0.975,1.016] | 1.044 [1.004,1.061] |
| `sqrt_scale100_x5000` | 0.959 [0.954,0.977] | 1.054 [1.049,1.058] |
| `sqrt_scale1000_x50` | 1.007 [0.998,1.023] | 1.015 [1.003,1.019] |
| `sqrt_scale5000_x5` | 1.004 [0.996,1.020] | 1.000 [0.996,1.010] |
| `multiply_dense_1000d_x5000` | 1.012 [1.000,1.016] | 1.014 [1.010,1.026] |
| `multiply_dense_10000d_x500` | 0.994 [0.981,1.007] | 1.006 [0.996,1.020] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.022 [1.014,1.031] | 0.999 [0.977,1.020] |
| `multiply_sparse_10000d_x1000` | 1.003 [0.992,1.014] | 1.003 [0.993,1.009] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.044 [1.012,1.068] | 1.080 [1.020,1.151] |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.000 [0.994,1.019] | 1.003 [0.992,1.049] |
| `modulo_1000d_350d_x3000` | 1.017 [0.998,1.033] | 1.014 [1.003,1.043] |
| `power_100d_e17_x1000` | 1.003 [0.986,1.022] | 1.022 [1.005,1.030] |
| `power_20d_e32_x2000` | 0.958 [0.935,1.006] | 1.103 [1.054,1.459] |
| `ibase_literal_flip_20000` | 0.968 [0.930,1.018] | 1.506 [1.416,1.532] |
| `recursion_50_x1000` | 0.985 [0.966,0.992] | 1.290 [1.258,1.298] |
| `sparse_array_200000` | 0.984 [0.967,0.985] | 1.665 [1.646,1.682] |
| `frontend_definitions_2000` | 0.996 [0.988,1.018] | 0.998 [0.988,1.010] |
| `decimal_input_4000d_x100` | 0.998 [0.985,1.019] | 0.998 [0.980,1.033] |
| `decimal_output_200d_x1000` | 1.010 [0.986,1.025] | 1.026 [1.019,1.048] |
| `streaming_statements_20000` | 1.007 [0.987,1.015] | 1.006 [0.999,1.026] |
| `square_100d_x100000` | 0.966 [0.955,0.976] | 1.232 [1.214,1.239] |
| `square_1000d_x10000` | 0.451 [0.450,0.538] | 1.008 [1.002,1.355] |
| `square_10000d_x500` | 0.567 [0.557,0.572] | 1.002 [0.991,1.013] |
| `literal_repeat_2000d_x2000` | 1.083 [1.065,1.157] | 1.115 [1.079,1.163] |
| `binary_output_1000d_x300` | 1.007 [0.962,1.013] | 1.012 [0.987,1.023] |
| `octal_output_1000d_x300` | 0.992 [0.977,1.004] | 1.007 [0.994,1.023] |
| `binary_input_100000d_x3` | 0.998 [0.989,1.006] | 1.020 [1.008,1.028] |
| `hex_input_100000d_x3` | 1.006 [0.991,1.020] | 1.007 [0.996,1.021] |
| `temporary_chain_1000d_x50000` | 1.449 [1.442,1.461] | 1.045 [1.042,1.062] |

## holdout paired intervals

| Workload | Unique / hybrid, CI95 | Heap COW / hybrid, CI95 |
| --- | --- | --- |
| `startup` | 1.022 [0.990,1.118] | 1.006 [0.914,1.150] |
| `loop_875000` | 0.981 [0.971,0.994] | 1.727 [1.705,1.756] |
| `calls_175000` | 0.976 [0.968,1.000] | 1.464 [1.444,1.504] |
| `array_175000` | 0.989 [0.976,1.000] | 1.602 [1.582,1.621] |
| `compare_high_1125d_x87500` | 1.308 [1.299,1.351] | 1.749 [1.718,1.786] |
| `compare_low_1125d_x87500` | 1.216 [1.204,1.234] | 1.529 [1.496,1.547] |
| `compare_equal_1125d_x87500` | 1.217 [1.195,1.230] | 1.532 [1.509,1.551] |
| `fibonacci_1125_x88` | 1.800 [1.759,1.818] | 1.385 [1.366,1.400] |
| `fibonacci_11250_x9` | 1.603 [1.595,1.627] | 1.205 [1.194,1.220] |
| `factorial_1125_x44` | 1.144 [1.127,1.154] | 1.183 [1.176,1.195] |
| `factorial_5625_x5` | 1.083 [1.077,1.089] | 1.045 [1.037,1.058] |
| `multiply_112d_x87500` | 1.287 [1.284,1.307] | 1.298 [1.278,1.303] |
| `multiply_1125d_x8750` | 1.022 [1.006,1.027] | 1.020 [1.009,1.043] |
| `multiply_11250d_x875` | 1.006 [0.998,1.007] | 1.018 [1.008,1.023] |
| `divide_scale112_x87500` | 1.078 [1.056,1.095] | 1.654 [1.611,1.680] |
| `divide_scale1125_x8750` | 1.009 [0.955,1.029] | 1.167 [1.088,1.181] |
| `divide_scale5625_x4375` | 1.009 [0.997,1.018] | 1.053 [1.021,1.064] |
| `sqrt_scale112_x4375` | 0.965 [0.958,0.975] | 1.048 [1.045,1.056] |
| `sqrt_scale1125_x44` | 1.001 [0.988,1.011] | 1.012 [0.997,1.022] |
| `sqrt_scale5625_x5` | 0.997 [0.993,1.004] | 0.999 [0.997,1.009] |
| `multiply_dense_1125d_x4375` | 1.010 [1.001,1.022] | 1.020 [1.013,1.023] |
| `multiply_dense_11250d_x438` | 1.012 [0.990,1.018] | 1.007 [0.997,1.026] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.023 [0.991,1.029] | 1.009 [1.001,1.029] |
| `multiply_sparse_11250d_x875` | 1.007 [1.003,1.039] | 1.006 [1.002,1.012] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.007 [0.952,1.112] | 1.022 [0.982,1.074] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.007 [0.981,1.040] | 1.028 [0.994,1.040] |
| `modulo_1125d_393d_x2625` | 1.067 [0.989,1.086] | 1.027 [0.967,1.074] |
| `power_112d_e17_x875` | 1.005 [0.974,1.031] | 1.016 [0.990,1.030] |
| `power_22d_e32_x1750` | 0.987 [0.898,0.995] | 1.081 [0.986,1.091] |
| `ibase_literal_flip_17500` | 0.956 [0.951,0.971] | 1.466 [1.453,1.484] |
| `recursion_56_x875` | 0.968 [0.954,0.985] | 1.247 [1.216,1.258] |
| `sparse_array_175000` | 0.976 [0.967,0.990] | 1.636 [1.629,1.667] |
| `frontend_definitions_1750` | 1.004 [0.994,1.019] | 0.989 [0.977,1.007] |
| `decimal_input_4500d_x88` | 1.003 [0.982,1.011] | 0.997 [0.978,1.003] |
| `decimal_output_225d_x875` | 1.010 [0.971,1.039] | 0.999 [0.993,1.109] |
| `streaming_statements_17500` | 1.001 [0.984,1.020] | 1.020 [1.003,1.027] |
| `square_112d_x87500` | 0.915 [0.901,0.934] | 1.205 [1.197,1.213] |
| `square_1125d_x8750` | 0.444 [0.440,0.450] | 1.014 [1.013,1.015] |
| `square_11250d_x438` | 0.523 [0.521,0.531] | 1.000 [0.981,1.003] |
| `literal_repeat_2250d_x1750` | 1.164 [1.075,1.277] | 1.127 [1.080,1.148] |
| `binary_output_1125d_x263` | 0.999 [0.980,1.022] | 0.995 [0.987,1.012] |
| `octal_output_1125d_x263` | 1.006 [0.978,1.018] | 1.009 [0.998,1.026] |
| `binary_input_112500d_x3` | 0.993 [0.991,1.005] | 1.015 [1.006,1.026] |
| `hex_input_112500d_x3` | 1.002 [0.986,1.015] | 1.011 [0.987,1.020] |
| `temporary_chain_1125d_x43750` | 1.398 [1.385,1.413] | 1.043 [1.039,1.044] |
