# Equal-scale mutable arithmetic — second #201 experiment

The equal-scale mutable iterator path sizes the destination once before creating
iterators and consumes source limbs directly. Once the source is exhausted, it
visits higher destination limbs only while carry/borrow remains. The existing
add/subtract policies retain their arithmetic, sign and normalization rules.
The differing-scale path remains generic and unchanged. Capturing the source
extent before resizing preserves self-aliases; callers retain COW isolation.
No architecture-specific implementation or hand-written reciprocal is added.

All 131 bc cases pass 580,334 assertions. Six CLI/oracle/component groups pass,
including the 33-case quick corpus. An extensions-disabled ASan/UBSan run passes
91 number/VM cases and 396,125 assertions. New 8/16/32-bit trait tests exercise
full carry/borrow chains, shorter destinations, signs, cancellation, self-aliases
and preserved shared copies. This is an optimization, not a claimed bug fix.

## Matched source builds

Parent: `2ddf749e8b313f620ad248d680e801c409bbd3f5` (merged ownership step #309).
Candidate: `a31a5b20f329552ebf8eeb86a23cfbd3c50c591b`.
Both immutable release recipes use fixed threshold 126, identical Apple Clang
21 compiler identities, libc++ header 220106, extensions ON, Release `-O3
-DNDEBUG`, thin LTO and C++20 on the recorded Apple M1/macOS 27.2 host. Actual
commands and binary hashes remain in the manifests. Path-dependent command
hashes differ. Howard 7.0.3 at c8dabac is the pinned native competitive reference.
No instrumented executable is timed. No local builds/profiling ran during
timing; the shared workstation has no CPU/frequency/thermal isolation.

Each full/held-out experiment has one checked warm-up and nine paired rounds per
case, seeded interleaving and 2,000 paired bootstrap resamples. All 990 full and
990 held-out invocations pass their independent output/status/stderr/deadline
checks. Full seed 197200 and held-out seed 197201 use the recorded generator
hashes. Held-out dimensions were reserved from implementation choices.

Intervals describe this host/sample only; small excursions are not universal
regressions or speedups. The earlier A/A results caution against interpreting
every nominal 95% excursion as material. No aggregate hides losing classes.
Native CI correctness is separate from native #216 performance trends.

## Results

Long Fibonacci takes 82.6% of parent time on the full shape and 81.4% on the
held-out shape. Short Fibonacci takes 97.4%/97.3%, with the held-out interval
reaching 1.000. The 100-digit winner is unchanged within noise. Large sparse
products take 94.3%/94.5%. Loop performance is essentially unchanged. Dense
array time is 1.9% higher in the full sample (0.9% held-out, whose interval
crosses 1); calls are 0.4%/1.0% higher. These small losing observations remain
visible; no blanket no-regression claim or competitive completion is made.

Ratios below 1 favor the candidate. Every workload, including neutral and
slower cases, remains in the tables and raw samples. Competitive targets remain
unmet in several classes; #201 and the umbrella are not closed by this step.

### Full

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 1.005 [0.914, 1.072] | 1.058 [0.999, 1.130] |
| `loop_1000000` | 0.998 [0.980, 1.002] | 2.046 [2.023, 2.049] |
| `calls_200000` | 1.004 [0.999, 1.006] | 2.422 [2.411, 2.434] |
| `array_200000` | 1.019 [1.009, 1.028] | 2.143 [2.126, 2.162] |
| `fibonacci_1000_x100` | 0.974 [0.961, 0.983] | 1.132 [1.126, 1.147] |
| `fibonacci_10000_x10` | 0.826 [0.823, 0.829] | 1.197 [1.194, 1.200] |
| `factorial_1000_x50` | 1.001 [0.992, 1.005] | 1.643 [1.634, 1.654] |
| `factorial_5000_x5` | 0.998 [0.997, 1.004] | 1.664 [1.659, 1.672] |
| `multiply_100d_x100000` | 0.997 [0.978, 1.007] | 0.918 [0.903, 0.931] |
| `multiply_1000d_x10000` | 1.001 [0.991, 1.001] | 2.083 [2.081, 2.089] |
| `multiply_10000d_x1000` | 0.979 [0.976, 0.980] | 7.232 [7.175, 7.299] |
| `divide_scale100_x100000` | 1.003 [0.993, 1.010] | 1.405 [1.389, 1.412] |
| `divide_scale1000_x10000` | 0.998 [0.996, 1.019] | 0.873 [0.862, 0.880] |
| `divide_scale5000_x5000` | 1.000 [0.972, 1.005] | 0.722 [0.719, 0.724] |
| `sqrt_scale100_x5000` | 0.993 [0.984, 1.003] | 0.637 [0.635, 0.639] |
| `sqrt_scale1000_x50` | 0.984 [0.980, 0.996] | 0.910 [0.906, 0.916] |
| `sqrt_scale5000_x5` | 0.993 [0.986, 0.997] | 0.852 [0.848, 0.854] |
| `multiply_dense_1000d_x5000` | 1.000 [0.996, 1.003] | 1.032 [1.030, 1.033] |
| `multiply_dense_10000d_x500` | 0.969 [0.947, 0.982] | 0.938 [0.934, 0.970] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.992 [0.989, 1.001] | 0.777 [0.774, 0.781] |
| `multiply_sparse_10000d_x1000` | 0.943 [0.939, 0.948] | 7.899 [7.898, 7.990] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.040 [0.970, 1.061] | 0.912 [0.903, 0.970] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.991 [0.983, 1.011] | 0.974 [0.968, 0.989] |
| `modulo_1000d_350d_x3000` | 1.012 [0.980, 1.037] | 0.752 [0.744, 0.766] |
| `power_100d_e17_x1000` | 0.994 [0.990, 1.000] | 2.287 [2.285, 2.304] |
| `power_20d_e32_x2000` | 0.998 [0.994, 1.004] | 2.178 [2.116, 2.188] |
| `ibase_literal_flip_20000` | 0.985 [0.948, 1.011] | 1.651 [1.620, 1.679] |
| `recursion_50_x1000` | 1.005 [0.999, 1.019] | 2.338 [2.290, 2.387] |
| `sparse_array_200000` | 0.999 [0.989, 1.005] | 1.940 [1.904, 1.945] |
| `frontend_definitions_2000` | 1.003 [0.994, 1.020] | 3.470 [3.345, 3.586] |
| `decimal_input_4000d_x100` | 0.995 [0.983, 1.000] | 33.652 [31.546, 34.746] |
| `decimal_output_200d_x1000` | 0.996 [0.968, 1.007] | 2.158 [2.080, 2.229] |
| `streaming_statements_20000` | 1.007 [0.997, 1.015] | 3.027 [2.947, 3.051] |

### Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 0.972 [0.880, 1.213] | 1.037 [0.863, 1.206] |
| `loop_875000` | 0.995 [0.984, 0.998] | 2.031 [2.000, 2.037] |
| `calls_175000` | 1.010 [1.003, 1.033] | 2.432 [2.396, 2.451] |
| `array_175000` | 1.009 [0.986, 1.024] | 2.134 [2.088, 2.141] |
| `fibonacci_1125_x88` | 0.973 [0.955, 1.000] | 1.147 [1.136, 1.178] |
| `fibonacci_11250_x9` | 0.814 [0.811, 0.826] | 1.208 [1.204, 1.231] |
| `factorial_1125_x44` | 1.005 [0.998, 1.010] | 1.648 [1.642, 1.655] |
| `factorial_5625_x5` | 0.998 [0.987, 1.006] | 1.677 [1.666, 1.682] |
| `multiply_112d_x87500` | 1.001 [0.994, 1.007] | 0.872 [0.868, 0.877] |
| `multiply_1125d_x8750` | 0.999 [0.994, 1.002] | 2.337 [2.323, 2.357] |
| `multiply_11250d_x875` | 0.980 [0.978, 0.983] | 7.860 [7.758, 7.917] |
| `divide_scale112_x87500` | 0.991 [0.977, 0.999] | 1.358 [1.339, 1.362] |
| `divide_scale1125_x8750` | 0.990 [0.969, 1.000] | 0.870 [0.861, 0.873] |
| `divide_scale5625_x4375` | 1.001 [0.997, 1.018] | 0.720 [0.715, 0.735] |
| `sqrt_scale112_x4375` | 0.997 [0.992, 1.025] | 0.677 [0.671, 0.697] |
| `sqrt_scale1125_x44` | 0.991 [0.980, 0.999] | 0.898 [0.883, 0.903] |
| `sqrt_scale5625_x5` | 0.991 [0.989, 1.000] | 0.847 [0.842, 0.863] |
| `multiply_dense_1125d_x4375` | 1.000 [0.984, 1.003] | 1.061 [1.060, 1.070] |
| `multiply_dense_11250d_x438` | 0.981 [0.979, 0.983] | 0.974 [0.937, 0.977] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.999 [0.996, 1.004] | 0.688 [0.684, 0.692] |
| `multiply_sparse_11250d_x875` | 0.945 [0.939, 0.948] | 8.404 [8.342, 8.477] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.016 [0.879, 1.114] | 0.672 [0.644, 0.735] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.003 [0.975, 1.006] | 1.287 [1.280, 1.296] |
| `modulo_1125d_393d_x2625` | 0.975 [0.900, 1.047] | 0.564 [0.558, 0.581] |
| `power_112d_e17_x875` | 0.998 [0.986, 0.998] | 2.410 [2.399, 2.422] |
| `power_22d_e32_x1750` | 1.006 [0.986, 1.018] | 2.365 [2.286, 2.397] |
| `ibase_literal_flip_17500` | 1.009 [0.990, 1.033] | 1.662 [1.645, 1.672] |
| `recursion_56_x875` | 1.000 [0.996, 1.003] | 2.339 [2.278, 2.358] |
| `sparse_array_175000` | 1.008 [0.997, 1.026] | 1.931 [1.919, 1.936] |
| `frontend_definitions_1750` | 1.002 [0.975, 1.098] | 3.303 [3.234, 3.420] |
| `decimal_input_4500d_x88` | 0.996 [0.992, 1.000] | 36.864 [34.812, 37.563] |
| `decimal_output_225d_x875` | 1.003 [0.943, 1.013] | 2.138 [2.075, 2.169] |
| `streaming_statements_17500` | 1.006 [0.993, 1.011] | 3.045 [2.995, 3.078] |

## Interpretation and remaining work

The independent ownership report remains the first #201 step; this report
isolates the additional aligned loop. General-scale arithmetic and unchanged
VM/allocation traffic still contribute to end-to-end time. Zero/length checks
cannot assume universal normalization: temporary ranges, padded fractions and
`mac(0, 0)` can retain leading zero limbs. Comparison and normalization decisions
are tracked separately rather than silently folded into this measurement.
