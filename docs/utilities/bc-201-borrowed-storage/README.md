# Borrowed digit storage — first #201 experiment

The mutable iterator helper now takes `DigitVector&` instead of a copied shared
owner. The calling BasicDigits retains the container throughout the check.
Vector element reallocation preserves the vector object and the helper already
returns the resulting iterator. The callers' COW, scale alignment, carry/borrow,
normalization and arithmetic policy are unchanged. The generic C++ path applies
to every provided limb trait; no architecture-specific implementation is added.

This is the first independently measured #201 change. Equal-scale loop/reserve,
comparison and normalization/zero-check decisions remain separate work, and
#201 stays open. Most loop-heavy VM work and whole-vector COW traffic remain.

## Correctness and source identity

All 128 bc cases pass 580,301 assertions in Release. The selected eight fixed126
CLI/helper groups pass, including exact arithmetic, VM and 32 quick corpus cases.
An extensions-disabled ASan/UBSan build passes 396,092 assertions across 88
number/VM cases. Four real diagnostic CLI cases pass, retaining COW/array
isolation, EOF balance and report-error checks. Existing carry/borrow chains,
self-aliases, shared copies, signs, zero and partial-limb scales remain covered.

The immutable release baseline is `10d6dc7655b736bb53033b844f70a48e2bbd9b81`;
the candidate is `fd902d0a51ec44d0319cd3b0b00eef66c3e8d221`. Both are built through
the same source recipe with identical compiler identities, header-library
markers, selected flags and fixed threshold 126. Only compile-command hashes
necessarily differ with their source/build paths. Both use extensions ON,
Apple Clang 21, libc++ header 220106, Release `-O3 -DNDEBUG`, thin LTO and C++20.
The host is the same Apple M1/macOS 27.2 as the recorded baseline. Native Howard
7.0.3 at c8dabac is the independent competitive reference, with its recorded
source-build flags and threshold 32. Instrumented binaries never enter timings.

## Paired observations

Each experiment uses one checked warm-up and nine measured paired rounds per
workload, fixed-seed interleaving and 2,000 paired bootstrap resamples. All 960
full-case and 990 held-out invocations pass status/stderr/deadline/output gates.
The full preset uses seed 197200. Held-out shapes and seed 197201 were reserved
from implementation choices; their generator is at
`742a7b147eca7154db1b96d70e45a1a65b3645f3` (#308), preserving all original full
program/oracle hashes and adding streaming throughput. The recorded harness
checksums identify each as-run generator. Startup is a shared control.

No local builds or profiling ran during measurement. This is a shared
workstation without CPU/thermal isolation; these observations do not establish
Linux/Windows speedups. The earlier A/A check also warns against interpreting
small nominally significant excursions as material regressions. Native CI gates
correctness, and #216 owns advisory native base/candidate trends.

Long Fibonacci takes 41.6% of parent time on the historical shape and 40.4% on
its held-out shape. Short Fibonacci takes 79.3% and 77.7%. The historical large
product takes 67.9%; held-out larger products take roughly 70–74%. The existing
100-digit product is essentially unchanged. Loop performance is unchanged, so
this alone does not satisfy #201's complete acceptance or #197's competitive
ambition. COW payload traffic is also unchanged. The 10%-faster-than-Howard goal
remains unmet in several classes; keep all misses visible.

Ratios below 1 favor the candidate. Intervals are approximate paired 95% bootstrap
intervals, not guarantees about another host. No aggregate/weights are produced.
All raw rows, provenance and per-implementation medians/min/max/MAD are retained.

### Historical full shapes

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 1.038 [0.986, 1.174] | 1.100 [0.905, 1.171] |
| `loop_1000000` | 0.999 [0.987, 1.081] | 2.060 [2.053, 2.117] |
| `calls_200000` | 0.991 [0.983, 0.993] | 2.435 [2.427, 2.457] |
| `array_200000` | 0.980 [0.967, 1.009] | 2.126 [2.102, 2.168] |
| `fibonacci_1000_x100` | 0.793 [0.786, 0.809] | 1.185 [1.172, 1.191] |
| `fibonacci_10000_x10` | 0.416 [0.415, 0.418] | 1.450 [1.443, 1.468] |
| `factorial_1000_x50` | 0.997 [0.939, 1.008] | 1.643 [1.637, 1.653] |
| `factorial_5000_x5` | 1.002 [0.997, 1.003] | 1.671 [1.665, 1.677] |
| `multiply_100d_x100000` | 0.999 [0.995, 1.002] | 0.905 [0.896, 0.918] |
| `multiply_1000d_x10000` | 1.000 [1.000, 1.004] | 2.086 [2.080, 2.097] |
| `multiply_10000d_x1000` | 0.679 [0.678, 0.680] | 7.423 [7.139, 7.473] |
| `divide_scale100_x100000` | 0.994 [0.980, 1.004] | 1.404 [1.399, 1.417] |
| `divide_scale1000_x10000` | 0.996 [0.991, 1.019] | 0.873 [0.870, 0.889] |
| `divide_scale5000_x5000` | 0.997 [0.990, 1.011] | 0.721 [0.715, 0.731] |
| `sqrt_scale100_x5000` | 0.898 [0.890, 0.902] | 0.636 [0.635, 0.637] |
| `sqrt_scale1000_x50` | 0.993 [0.991, 1.008] | 0.919 [0.917, 0.926] |
| `sqrt_scale5000_x5` | 1.003 [0.999, 1.009] | 0.857 [0.855, 0.858] |
| `multiply_dense_1000d_x5000` | 1.000 [0.996, 1.003] | 1.031 [1.025, 1.035] |
| `multiply_dense_10000d_x500` | 0.682 [0.679, 0.687] | 0.973 [0.970, 0.977] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.009 [1.000, 1.012] | 0.784 [0.780, 0.789] |
| `multiply_sparse_10000d_x1000` | 0.713 [0.711, 0.714] | 8.473 [8.449, 8.549] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.035 [0.969, 1.090] | 0.896 [0.873, 0.947] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.996 [0.980, 1.031] | 0.964 [0.957, 1.004] |
| `modulo_1000d_350d_x3000` | 0.942 [0.937, 0.949] | 0.739 [0.737, 0.742] |
| `power_100d_e17_x1000` | 0.902 [0.895, 0.904] | 2.313 [2.215, 2.320] |
| `power_20d_e32_x2000` | 0.999 [0.995, 1.001] | 2.217 [2.196, 2.231] |
| `ibase_literal_flip_20000` | 1.005 [1.000, 1.011] | 1.677 [1.660, 1.698] |
| `recursion_50_x1000` | 1.007 [0.996, 1.030] | 2.399 [2.366, 2.430] |
| `sparse_array_200000` | 0.991 [0.978, 0.998] | 1.933 [1.918, 1.965] |
| `frontend_definitions_2000` | 0.989 [0.984, 0.996] | 3.472 [3.409, 3.572] |
| `decimal_input_4000d_x100` | 0.996 [0.994, 0.998] | 33.929 [31.086, 35.147] |
| `decimal_output_200d_x1000` | 1.007 [0.992, 1.041] | 2.206 [2.086, 2.219] |

### Held-out shapes

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 1.014 [0.948, 1.035] | 1.037 [0.998, 1.053] |
| `loop_875000` | 0.984 [0.981, 0.988] | 2.032 [2.023, 2.036] |
| `calls_175000` | 0.987 [0.976, 0.991] | 2.437 [2.394, 2.445] |
| `array_175000` | 0.973 [0.962, 1.000] | 2.101 [2.091, 2.130] |
| `fibonacci_1125_x88` | 0.777 [0.764, 0.785] | 1.173 [1.158, 1.181] |
| `fibonacci_11250_x9` | 0.404 [0.402, 0.411] | 1.488 [1.473, 1.509] |
| `factorial_1125_x44` | 1.001 [0.992, 1.006] | 1.653 [1.644, 1.663] |
| `factorial_5625_x5` | 0.999 [0.997, 1.000] | 1.665 [1.660, 1.672] |
| `multiply_112d_x87500` | 0.994 [0.987, 1.001] | 0.870 [0.865, 0.873] |
| `multiply_1125d_x8750` | 0.999 [0.992, 1.000] | 2.345 [2.337, 2.357] |
| `multiply_11250d_x875` | 0.702 [0.701, 0.704] | 8.065 [8.047, 8.102] |
| `divide_scale112_x87500` | 0.991 [0.979, 0.996] | 1.349 [1.341, 1.361] |
| `divide_scale1125_x8750` | 0.993 [0.987, 1.011] | 0.870 [0.860, 0.873] |
| `divide_scale5625_x4375` | 1.000 [0.991, 1.003] | 0.719 [0.709, 0.721] |
| `sqrt_scale112_x4375` | 0.905 [0.903, 0.910] | 0.675 [0.674, 0.677] |
| `sqrt_scale1125_x44` | 0.991 [0.964, 0.996] | 0.904 [0.900, 0.907] |
| `sqrt_scale5625_x5` | 1.006 [1.005, 1.014] | 0.853 [0.850, 0.856] |
| `multiply_dense_1125d_x4375` | 0.999 [0.996, 1.003] | 1.062 [1.060, 1.064] |
| `multiply_dense_11250d_x438` | 0.708 [0.707, 0.712] | 1.002 [0.996, 1.007] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.997 [0.988, 1.005] | 0.687 [0.674, 0.691] |
| `multiply_sparse_11250d_x875` | 0.735 [0.733, 0.738] | 8.898 [8.820, 9.008] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.021 [0.991, 1.152] | 0.719 [0.693, 0.739] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.041 [1.030, 1.046] | 1.319 [1.307, 1.333] |
| `modulo_1125d_393d_x2625` | 0.997 [0.963, 1.058] | 0.599 [0.565, 0.620] |
| `power_112d_e17_x875` | 0.908 [0.904, 0.912] | 2.427 [2.417, 2.430] |
| `power_22d_e32_x1750` | 1.001 [0.999, 1.014] | 2.362 [2.282, 2.378] |
| `ibase_literal_flip_17500` | 1.003 [0.995, 1.027] | 1.658 [1.607, 1.702] |
| `recursion_56_x875` | 0.997 [0.994, 1.009] | 2.358 [2.284, 2.378] |
| `sparse_array_175000` | 0.990 [0.985, 1.002] | 1.935 [1.920, 1.946] |
| `frontend_definitions_1750` | 0.998 [0.980, 1.032] | 3.372 [3.289, 3.440] |
| `decimal_input_4500d_x88` | 0.997 [0.996, 1.000] | 37.697 [36.982, 38.745] |
| `decimal_output_225d_x875` | 0.993 [0.978, 1.018] | 2.148 [2.129, 2.174] |
| `streaming_statements_17500` | 1.004 [0.992, 1.013] | 3.070 [2.995, 3.105] |

## Mechanism and remaining costs

The matched release `for_each` excerpt shrinks from 186 to 130 lines of symbol/
instruction output. Its four `ldadd`/`ldaddal` instructions (two owner-copy paths)
are absent in the candidate excerpt. This supports the source change's mechanism;
it does not assign a fixed number of cycles to each owner operation. Removing
synchronization also permits different compiler optimizations and layout.
The binary's `__text` section changes from 227,884 to 227,732 bytes; the aligned
`__TEXT` segment remains 278,528 bytes. Native raw excerpts/size reports are kept.

The fixed126 long-Fibonacci diagnostic reports zero iterator-owner-copy events,
zero tracked live bytes at EOF, and the same 47,021,676 COW payload bytes as the
prior profile. Both basic and Karatsuba multiply-call counts are zero for this
addition-only workload, so the earlier diagnostic threshold 191 cannot affect
that comparison. The historical event key remains in schema 1, with zero for
the borrowed check; other owner-copy and allocation events remain active.
This is software-site attribution, not a hardware atomic counter or a timed
instrumented result. Immutable input/value semantics and the remaining large
COW/allocation costs still require separate investigation.
