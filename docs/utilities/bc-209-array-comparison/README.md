# Matched array storage comparison

Issue #209 compares the existing hybrid layout with portable paged, sorted
flat, hash and ordered-map alternatives through the ordinary interpreter.
The production default remains hybrid. This archive preserves every full and
held-out workload, exact oracle, checked observation, recipe and diagnostic;
there is no aggregate performance pass or automatic default-selection rule.

## Identities and protocol

All six GD Release builds use immutable revision
`7723ebd74614df1a56e03cb5867ef53efcdd0104`, Apple Clang 21, extensions ON and
multiplication split point 126. The `aa` role is an independent hybrid build.
Howard is pinned at `c8dabac65d39fd527a9264a5e505a99b1d8df3d8`, without `-l`.
Actual compiler/runtime-header identities, flags, executable hashes, CMake
commands and dependencies are in the recipes. This historical runtime predates
the generic decimal-division cancellation fix; measurements are not relabeled
as a later source revision when the experiment is integrated.

The frozen corpus has eight classes per preset: sequential dense writes,
permuted dense writes, random sparse writes with absent neighbors, clustered
sparse writes, repeated high-index replacement, absent reads, array-parameter
snapshots and heap-sized coefficients. Held-out dimensions, permutation and
sparse-key parameters differ. Inputs and independently derived exact integer
oracles are identical across roles. There are 15 randomized paired rounds and
three warmups per class. Both A/A stages precede their matching comparison;
one bounded confirmation is permitted for an A/A interval wholly outside
either direction of the five-percent band. All control qualifications remain
in `paired-release.zip`, including inconclusive results.

The first attempt used the harness's default 30-second child deadline and
failed during a sorted-flat permuted-write warmup. `initial-timeout30.zip`
preserves that failure and its incomplete observations. The completed run uses
a fixed 120-second child deadline for every implementation, with identical
inputs and sample counts. The failed attempt is never pooled into accepted
summaries. Builds, correctness tests and resource collection finished before
the restarted timing run. No local builds, tests, profiling or resource jobs
ran concurrently with it.

## Diagnostics and correctness

`software-counters.zip` contains 40 checked instrumented invocations: all five
layouts across the eight quick inputs. Every report balances to zero live
bytes. These are logical allocator traffic/peak-live bytes, excluding allocator
metadata; they are not elapsed measurements or process RSS and are not
extrapolated from the quick corpus to full dimensions.

`native-resources.zip` contains 120 checked full-input observations: five
layouts, eight classes and three randomized-order trials. Darwin peak RSS is
in bytes. The maintained helper's wrapper timings and optional hardware
counters are separate diagnostics, not paired elapsed results. Collectors and
exact inputs remain in the archive. RSS includes interpreter/runtime overhead.

Each of the four selected alternative layouts passed the complete local
ASan/UBSan unit suite (218 cases, 1,026,564 assertions per layout), quick
exact-oracle interpreter cases and CLI/VM subprocess tests. The hybrid build
passed its dedicated array suite (six cases, 378,968 assertions) and quick
interpreter checks; native CI separately exercises the default full suite. Leak detection is unavailable on this host and
was disabled explicitly. Native CI compiles and exercises the shared storage
oracles under Linux, Windows and macOS, in both extension modes. The shared
suite retains NumberTraits8/16/32 and checks sparse maxima, page boundaries,
absent reads, written zero, highest index, copy/move and borrowed insertion.
The high-index CLI corpus is explicitly limited to native 64-bit hosts.

The native arithmetic artifacts compare their actual base/candidate revisions,
not another library source: all 6,000 observations are checked, with matching
source identities, corpus hashes and sample counts, and no confirmed
regression. Native math jobs correctly report unavailable because this topic
predates the library. The full alternative-layout timing/RSS comparison is
from one native Apple ARM64 host; it is not three-platform performance proof.

## Decision

The default-selection review and per-class tables are appended only after the
complete full/held-out audit. Keep unfavorable observations and memory costs
visible. No container is selected merely because its asymptotic lookup bound
or one favorable workload is better. The alternatives remain explicit CMake
experiments with a portable generic hybrid default.

## Audited results

The completed run has 2,304 checked elapsed observations. Fourteen A/A
controls pass; two are inconclusive. No confirmed control bias is reported.
Each ratio below is candidate/hybrid, followed by its paired 95% interval.
Ratios below one favor the candidate. These are per-class observations,
without an aggregate gate.

### Full elapsed ratios

| Class | Paged | Flat | Hash | Ordered | Howard |
|---|---:|---:|---:|---:|---:|
| array_layout_sequential_full | 1.173 [1.171, 1.202] | 1.202 [1.191, 1.213] | 1.050 [1.047, 1.056] | 1.193 [1.181, 1.202] | 1.072 [1.065, 1.073] |
| array_layout_permuted_dense_full | 1.030 [1.024, 1.043] | 301.183 [299.692, 302.944] | 0.985 [0.977, 1.025] | 1.104 [1.095, 1.168] | 0.955 [0.944, 0.963] |
| array_layout_sparse_full | 1.693 [1.647, 1.769] | 10.207 [9.668, 10.424] | 0.926 [0.904, 0.944] | 0.986 [0.940, 1.012] | 795.590 [778.916, 826.785] |
| array_layout_clustered_full | 0.967 [0.937, 0.994] | 27.252 [26.325, 27.728] | 0.948 [0.933, 0.988] | 1.001 [0.980, 1.028] | 138.884 [134.103, 144.768] |
| array_layout_overwrite_full | 0.987 [0.977, 0.997] | 1.010 [1.001, 1.016] | 1.015 [1.006, 1.028] | 0.998 [0.985, 1.005] | 262.153 [256.483, 263.164] |
| array_layout_absent_full | 1.015 [0.995, 1.044] | 1.019 [1.004, 1.037] | 1.076 [1.068, 1.079] | 1.012 [1.004, 1.024] | 90.906 [88.754, 93.357] |
| array_layout_snapshot_full | 1.013 [0.894, 1.056] | 1.017 [0.982, 1.040] | 1.880 [1.727, 1.922] | 2.253 [1.978, 2.287] | 1.845 [1.563, 1.938] |
| array_layout_heap_values_full | 1.064 [1.040, 1.075] | 1.113 [1.084, 1.132] | 1.050 [1.027, 1.053] | 1.129 [1.116, 1.132] | 0.949 [0.931, 0.958] |

### Holdout elapsed ratios

| Class | Paged | Flat | Hash | Ordered | Howard |
|---|---:|---:|---:|---:|---:|
| array_layout_sequential_holdout | 1.185 [1.169, 1.200] | 1.193 [1.186, 1.206] | 1.057 [1.050, 1.060] | 1.199 [1.194, 1.202] | 1.074 [1.061, 1.080] |
| array_layout_permuted_dense_holdout | 1.083 [1.055, 1.089] | 338.039 [330.603, 340.831] | 1.036 [1.000, 1.057] | 1.151 [1.132, 1.163] | 0.954 [0.929, 0.980] |
| array_layout_sparse_holdout | 1.353 [1.270, 1.401] | 14.455 [13.989, 14.727] | 0.939 [0.905, 0.999] | 1.004 [0.986, 1.026] | 724.811 [701.074, 731.181] |
| array_layout_clustered_holdout | 0.970 [0.951, 0.993] | 30.887 [30.529, 32.292] | 0.954 [0.945, 0.982] | 0.998 [0.967, 1.007] | 123.482 [120.334, 125.997] |
| array_layout_overwrite_holdout | 1.001 [0.968, 1.007] | 0.997 [0.987, 1.034] | 1.028 [1.014, 1.047] | 1.003 [0.975, 1.030] | 230.236 [223.803, 242.713] |
| array_layout_absent_holdout | 1.004 [0.988, 1.008] | 1.020 [1.007, 1.034] | 1.081 [1.074, 1.084] | 1.005 [1.000, 1.012] | 81.663 [80.818, 82.509] |
| array_layout_snapshot_holdout | 1.117 [0.952, 1.236] | 1.040 [0.910, 1.123] | 1.792 [1.703, 1.895] | 2.115 [1.962, 2.213] | 1.787 [1.569, 1.829] |
| array_layout_heap_values_holdout | 1.064 [1.054, 1.091] | 1.129 [1.110, 1.144] | 1.034 [1.031, 1.046] | 1.139 [1.102, 1.148] | 0.944 [0.924, 0.953] |

### Full-input peak RSS

Medians of three separate Darwin wrapper observations, in MiB (2^20 bytes).

| Class | Hybrid | Paged | Flat | Hash | Ordered |
|---|---:|---:|---:|---:|---:|
| array_layout_sequential_full | 23.156 | 14.219 | 23.156 | 17.453 | 17.344 |
| array_layout_permuted_dense_full | 34.672 | 14.203 | 23.141 | 17.453 | 17.359 |
| array_layout_sparse_full | 3.516 | 80.469 | 4.438 | 3.641 | 3.531 |
| array_layout_clustered_full | 4.453 | 3.969 | 4.953 | 4.797 | 4.484 |
| array_layout_overwrite_full | 1.922 | 1.984 | 1.922 | 1.938 | 1.953 |
| array_layout_absent_full | 1.922 | 1.953 | 1.906 | 1.938 | 1.953 |
| array_layout_snapshot_full | 2.531 | 2.484 | 2.562 | 2.672 | 2.672 |
| array_layout_heap_values_full | 7.391 | 6.422 | 7.422 | 7.266 | 6.906 |

Retain hybrid: sorted-flat permuted insertion is 301x/338x slower on the
full/held-out inputs; paged random-sparse writes are 1.69x/1.35x slower
and use roughly 22.9x hybrid RSS on the full sparse input. Hash wins some
sparse classes but snapshots are 1.88x/1.79x slower, and it loses on
sequential and absent-read cases. The ordered reference loses on dense
classes and snapshots. Paged clustered writes have a modest win, without
overcoming its sparse-page cost. These tradeoffs do not justify a global
replacement or a memory blowup. Howard also has wins and substantial
losses, all retained above; the overall competitive roadmap remains open.
