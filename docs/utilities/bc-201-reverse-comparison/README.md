# Most-significant equal-scale comparison — third #201 experiment

Equal-scale comparison now zero-extends operands and visits limbs from the
most significant end, returning at the first unequal limb. Differing scales
retain the general alignment path. The numeric result no longer depends on
scanning and overwriting the comparison result at every less significant limb.
The generic implementation applies to every provided trait without ISA dispatch.

## Representation and tests

Storage length alone is not numeric length. Range copies and fractional splits
can retain leading zero limbs; `mac(0, 0)` can retain nonempty all-zero storage.
Therefore constant-time empty-container/final-limb zero and length shortcuts are
not adopted. `tidy()` is an explicit operation, not a universal invariant; it
retains an empty owner. Zero-extension makes the comparison safe without adding
normalization scans to every producer or changing padding consumers. Tests
construct live untrimmed zero/one representations at all three limb widths and
check comparison, numeric zero and decimal length.

All 131 bc cases pass 580,340 assertions. The six comparison cases pass 6,132
assertions in Release and extensions-disabled ASan/UBSan; seven independent CLI
oracle groups pass. Fifteen runner/corpus groups pass, including 36 native quick
cases, retained historical hashes and distinct/replayable held-out shapes.
The quick check also passes all 108 invocations across the three implementations.

## Isolated source and measurements

Parent `2ddf749e8b313f620ad248d680e801c409bbd3f5` includes the first ownership step.
Candidate `6cf81f0b6bd84a6acccec1b81bc92e04089b4857` changes comparison and its
representation comments/tests. It does **not** include the aligned-arithmetic
patch measured in #310, so each micro-change remains independently attributable.
After the isolated experiment, merged #310 was integrated for review. The
combined tree passes 134 bc cases (580,373 assertions) and six CLI/oracle/
component groups, including 36 quick cases. The tables below retain the
original independent source identities; they do not relabel combined timings.

The as-run generator is `da2790f3ef0751f9afeb6f9c813983f6a0bc8cc0`, adding three
comparison shapes while preserving every historical program/oracle hash.

Immutable fixed126 recipes use identical Apple Clang21 compiler identities,
libc++ header220106, extensions ON, Release `-O3 -DNDEBUG`, thin LTO and C++20.
Native pinned Howard7.0.3 at c8dabac is the competitive reference. Actual command
and binary hashes remain in the manifests. Path-dependent command hashes differ.
The host is the recorded Apple M1/macOS27.2 workstation. No local builds or
profiling ran during timing; CPU/frequency/thermal isolation is unavailable.

All 1,080 full and 1,080 held-out invocations pass independent output, exit,
stderr and deadline gates. Each case has one checked warm-up and nine paired
measured rounds, seeded interleaving and 2,000 paired bootstrap resamples.
Full uses seed197200; held-out dimensions and seed197201 were reserved from
implementation choices. There is no aggregate or hidden exclusion. Native CI
correctness is separate from #216 advisory hosted-VM timing.

High-unequal comparisons take67.1% of parent time (64.4% held-out). Low-unequal
and equal comparisons take77.5%/77.2% (75.9%/76.2% held-out): even scans that
cannot return early avoid the general scaled traversal. Loop and Fibonacci
performance remain essentially unchanged. The existing small multiplication
winner stays unchanged within noise. Other neutral/losing observations remain
visible; this is not a blanket no-regression or cross-platform speed claim.
The earlier A/A experiment cautions against treating tiny nominal excursions
as universal effects. Broader competitive targets remain unmet in several classes.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 1.005 [0.979, 1.047] | 1.027 [1.004, 1.100] |
| `loop_1000000` | 0.997 [0.941, 1.006] | 2.048 [2.024, 2.065] |
| `calls_200000` | 1.000 [0.999, 1.005] | 2.411 [2.390, 2.424] |
| `array_200000` | 0.986 [0.972, 1.020] | 2.128 [2.093, 2.163] |
| `compare_high_1000d_x100000` | 0.671 [0.660, 0.681] | 2.193 [2.117, 2.207] |
| `compare_low_1000d_x100000` | 0.775 [0.767, 0.778] | 2.072 [2.066, 2.093] |
| `compare_equal_1000d_x100000` | 0.772 [0.761, 0.785] | 2.081 [2.048, 2.095] |
| `fibonacci_1000_x100` | 0.998 [0.996, 1.003] | 1.168 [1.166, 1.176] |
| `fibonacci_10000_x10` | 1.004 [1.000, 1.036] | 1.458 [1.449, 1.515] |
| `factorial_1000_x50` | 1.001 [0.997, 1.002] | 1.645 [1.639, 1.649] |
| `factorial_5000_x5` | 1.000 [0.997, 1.005] | 1.665 [1.662, 1.670] |
| `multiply_100d_x100000` | 1.005 [0.987, 1.017] | 0.918 [0.892, 0.926] |
| `multiply_1000d_x10000` | 0.998 [0.997, 1.020] | 2.087 [2.084, 2.121] |
| `multiply_10000d_x1000` | 0.999 [0.998, 1.025] | 7.401 [7.368, 7.643] |
| `divide_scale100_x100000` | 0.993 [0.954, 1.023] | 1.401 [1.399, 1.427] |
| `divide_scale1000_x10000` | 1.004 [0.987, 1.016] | 0.876 [0.846, 0.893] |
| `divide_scale5000_x5000` | 0.992 [0.964, 1.005] | 0.716 [0.711, 0.723] |
| `sqrt_scale100_x5000` | 0.980 [0.978, 0.989] | 0.625 [0.623, 0.631] |
| `sqrt_scale1000_x50` | 0.983 [0.981, 0.986] | 0.903 [0.900, 0.907] |
| `sqrt_scale5000_x5` | 0.983 [0.979, 0.990] | 0.845 [0.842, 0.849] |
| `multiply_dense_1000d_x5000` | 1.000 [0.998, 1.002] | 1.030 [1.020, 1.031] |
| `multiply_dense_10000d_x500` | 1.000 [0.994, 1.002] | 0.965 [0.960, 0.968] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.003 [0.997, 1.014] | 0.786 [0.781, 0.790] |
| `multiply_sparse_10000d_x1000` | 0.999 [0.995, 1.005] | 8.520 [8.416, 8.558] |
| `divide_multilimb_900d_450d_s100_x1000` | 0.957 [0.905, 1.001] | 0.870 [0.866, 0.947] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.989 [0.962, 1.007] | 0.958 [0.954, 0.967] |
| `modulo_1000d_350d_x3000` | 0.993 [0.986, 0.994] | 0.741 [0.736, 0.745] |
| `power_100d_e17_x1000` | 1.000 [0.998, 1.002] | 2.302 [2.276, 2.324] |
| `power_20d_e32_x2000` | 0.999 [0.989, 1.003] | 2.225 [2.210, 2.233] |
| `ibase_literal_flip_20000` | 0.996 [0.989, 1.012] | 1.659 [1.566, 1.695] |
| `recursion_50_x1000` | 1.005 [0.992, 1.030] | 2.381 [2.334, 2.474] |
| `sparse_array_200000` | 0.997 [0.994, 1.010] | 1.940 [1.927, 1.983] |
| `frontend_definitions_2000` | 1.006 [1.000, 1.015] | 3.522 [3.328, 3.568] |
| `decimal_input_4000d_x100` | 1.000 [0.996, 1.008] | 35.166 [31.368, 35.705] |
| `decimal_output_200d_x1000` | 0.987 [0.974, 0.998] | 2.194 [2.029, 2.257] |
| `streaming_statements_20000` | 0.999 [0.991, 1.010] | 3.049 [2.999, 3.063] |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 1.003 [0.896, 1.097] | 1.069 [1.024, 1.140] |
| `loop_875000` | 1.001 [0.993, 1.002] | 2.042 [2.008, 2.050] |
| `calls_175000` | 0.994 [0.978, 1.002] | 2.401 [2.330, 2.419] |
| `array_175000` | 1.008 [0.994, 1.015] | 2.127 [2.113, 2.159] |
| `compare_high_1125d_x87500` | 0.644 [0.631, 0.650] | 2.069 [2.026, 2.137] |
| `compare_low_1125d_x87500` | 0.759 [0.746, 0.765] | 2.056 [2.046, 2.096] |
| `compare_equal_1125d_x87500` | 0.762 [0.756, 0.784] | 2.041 [2.025, 2.068] |
| `fibonacci_1125_x88` | 0.999 [0.993, 1.002] | 1.172 [1.155, 1.184] |
| `fibonacci_11250_x9` | 1.000 [0.989, 1.003] | 1.489 [1.478, 1.501] |
| `factorial_1125_x44` | 0.997 [0.991, 0.998] | 1.646 [1.635, 1.655] |
| `factorial_5625_x5` | 1.002 [0.996, 1.012] | 1.670 [1.662, 1.673] |
| `multiply_112d_x87500` | 1.002 [0.963, 1.012] | 0.871 [0.866, 0.880] |
| `multiply_1125d_x8750` | 1.000 [0.943, 1.002] | 2.349 [2.347, 2.356] |
| `multiply_11250d_x875` | 0.998 [0.975, 1.000] | 8.086 [8.063, 8.110] |
| `divide_scale112_x87500` | 0.998 [0.988, 1.002] | 1.362 [1.340, 1.367] |
| `divide_scale1125_x8750` | 1.001 [0.988, 1.030] | 0.872 [0.845, 0.884] |
| `divide_scale5625_x4375` | 0.994 [0.957, 1.017] | 0.718 [0.712, 0.731] |
| `sqrt_scale112_x4375` | 0.980 [0.967, 0.987] | 0.660 [0.653, 0.665] |
| `sqrt_scale1125_x44` | 0.984 [0.979, 0.989] | 0.888 [0.885, 0.890] |
| `sqrt_scale5625_x5` | 0.987 [0.983, 0.990] | 0.843 [0.840, 0.847] |
| `multiply_dense_1125d_x4375` | 1.002 [0.996, 1.003] | 1.064 [1.061, 1.066] |
| `multiply_dense_11250d_x438` | 1.000 [0.994, 1.024] | 1.001 [0.991, 1.017] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.002 [0.883, 1.004] | 0.690 [0.685, 0.691] |
| `multiply_sparse_11250d_x875` | 1.001 [0.993, 1.005] | 8.999 [8.928, 9.181] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.019 [0.840, 1.087] | 0.633 [0.590, 0.683] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.984 [0.973, 0.998] | 1.267 [1.263, 1.287] |
| `modulo_1125d_393d_x2625` | 0.919 [0.888, 0.968] | 0.553 [0.550, 0.566] |
| `power_112d_e17_x875` | 1.001 [0.997, 1.008] | 2.427 [2.412, 2.439] |
| `power_22d_e32_x1750` | 0.999 [0.989, 1.011] | 2.370 [2.363, 2.380] |
| `ibase_literal_flip_17500` | 1.017 [0.990, 1.029] | 1.668 [1.641, 1.700] |
| `recursion_56_x875` | 1.002 [0.996, 1.014] | 2.339 [2.286, 2.365] |
| `sparse_array_175000` | 1.005 [1.003, 1.033] | 1.940 [1.893, 1.965] |
| `frontend_definitions_1750` | 1.011 [0.983, 1.049] | 3.350 [3.253, 3.394] |
| `decimal_input_4500d_x88` | 1.002 [0.987, 1.011] | 36.185 [35.614, 37.163] |
| `decimal_output_225d_x875` | 1.005 [0.915, 1.086] | 2.157 [2.080, 2.280] |
| `streaming_statements_17500` | 1.004 [0.999, 1.006] | 3.063 [2.976, 3.110] |
