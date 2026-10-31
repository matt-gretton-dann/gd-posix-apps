# bc baseline and attribution — 31 October 2026

This is the #200 baseline before architectural optimization for #197. It
reproduces the historical workload shapes with checked outputs and adds distinct
large-divisor, dense/sparse/unbalanced, power, recursion and conversion classes.
It makes no claim that all #197 performance targets have been achieved.

## Release experiment

The host is an Apple M1, arm64, macOS 27.2. GD source is
`6a1704e0297a4f045d3d5bd5deb5c1e8551d5ff1` (the VM lifetime repair), with Apple
Clang 21.0.0, libc++ `_LIBCPP_VERSION=220106`, C++20, `-O3 -DNDEBUG`, thin LTO,
extensions ON and multiplication split point 126. These runtime sources match
the merged lifetime repair; the later default-OFF diagnostic aliases do not
introduce an optimization. Compiler command provenance and binary hashes are
retained in the manifests and `gd-build-commands.txt`.

Howard 7.0.3 is built at `c8dabac65d39fd527a9264a5e505a99b1d8df3d8`, with the
recorded Clang compiler, `-O3 -DNDEBUG`, NLS/history/dc disabled, extra math ON,
and threshold 32. GNU 1.08.2 uses the pinned archive hash and recorded flags.
Apple's installed Howard is separate, with unknown source/compiler flags. It
is not the controlled reference. See the [build recipe](../bc-comparator-builds.md).

Each run uses seed/corpus seed 197200, one checked warm-up and nine checked
measurement rounds, randomized nearby implementation order and independent
integer/scaled/root oracles. All 1,280 competitive invocations and all 640 A/A
invocations passed exit, stderr, deadline and output checks. No local builds ran
during these timing experiments; this remains a shared workstation, without
CPU pinning, frequency/thermal control or a claim of identical external load.
These are observations on this host, not native Linux/Windows measurements.
#216 owns same-VM native base/candidate trends and cross-platform acceptance.

The table reports the median paired GD/Howard ratio and approximate 95% paired
percentile-bootstrap interval (2,000 seeded resamples). Below 1 means GD is
faster. The ambition is at most 0.90 for agreed substantive classes, with the
interval below 1; misses remain visible. No representative weights have been
agreed, so no aggregate score is reported. POSIX math is pending #217, never a
performance pass. Held-out inputs/classes remain mandatory for tuning and PGO.

| Workload | GD / Howard | Paired 95% interval |
| :--- | ---: | :--- |
| `startup` | 1.059 | [0.913, 1.111] |
| `loop_1000000` | 2.074 | [2.033, 2.092] |
| `calls_200000` | 2.477 | [2.437, 2.510] |
| `array_200000` | 2.155 | [2.131, 2.192] |
| `fibonacci_1000_x100` | 1.472 | [1.446, 1.520] |
| `fibonacci_10000_x10` | 3.501 | [3.467, 3.542] |
| `factorial_1000_x50` | 1.627 | [1.605, 1.697] |
| `factorial_5000_x5` | 1.663 | [1.653, 1.681] |
| `multiply_100d_x100000` | 0.914 | [0.912, 0.925] |
| `multiply_1000d_x10000` | 2.080 | [2.042, 2.091] |
| `multiply_10000d_x1000` | 10.983 | [10.636, 11.084] |
| `divide_scale100_x100000` | 1.421 | [1.410, 1.432] |
| `divide_scale1000_x10000` | 0.884 | [0.855, 0.911] |
| `divide_scale5000_x5000` | 0.724 | [0.711, 0.731] |
| `sqrt_scale100_x5000` | 0.705 | [0.687, 0.716] |
| `sqrt_scale1000_x50` | 0.942 | [0.908, 1.491] |
| `sqrt_scale5000_x5` | 0.856 | [0.843, 0.875] |
| `multiply_dense_1000d_x5000` | 1.029 | [1.002, 1.034] |
| `multiply_dense_10000d_x500` | 1.422 | [1.416, 1.431] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.775 | [0.772, 0.783] |
| `multiply_sparse_10000d_x1000` | 11.901 | [11.788, 12.004] |
| `divide_multilimb_900d_450d_s100_x1000` | 0.896 | [0.867, 0.910] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.947 | [0.944, 0.974] |
| `modulo_1000d_350d_x3000` | 0.788 | [0.779, 0.799] |
| `power_100d_e17_x1000` | 2.568 | [2.556, 2.584] |
| `power_20d_e32_x2000` | 2.214 | [2.106, 2.244] |
| `ibase_literal_flip_20000` | 1.652 | [1.540, 1.680] |
| `recursion_50_x1000` | 2.368 | [2.334, 2.449] |
| `sparse_array_200000` | 1.944 | [1.927, 1.985] |
| `frontend_definitions_2000` | 3.588 | [2.639, 3.656] |
| `decimal_input_4000d_x100` | 35.405 | [31.763, 37.700] |
| `decimal_output_200d_x1000` | 2.131 | [2.033, 2.231] |

The full manifests/summaries retain all four implementations, their per-case
medians, min/max/MAD, source/configuration and output hashes. CSV files preserve
every warm-up/measurement row, checked status, round and within-block order.
Programs/oracles can be regenerated from the pinned harness and corpus seed;
the manifest records their exact hashes.

## A/A noise check

The same GD executable was configured under two names. Most paired intervals
include 1, but the following nominal 95% intervals exclude it despite identical
code. These small excursions are not code regressions and do not justify hard
CI gates. Repeated A/A calibration on each native runner is still required.

| Identical-binary workload | Second / first | Paired 95% interval |
| :--- | ---: | :--- |
| `loop_1000000` | 0.997893 | [0.993507, 0.999201] |
| `divide_scale100_x100000` | 1.003390 | [1.000316, 1.008846] |
| `sqrt_scale100_x5000` | 1.003712 | [1.000908, 1.008630] |
| `decimal_input_4000d_x100` | 1.001471 | [1.000959, 1.003711] |

## Allocation and ownership observations

`allocation-profiles.json` retains all 32 checked full-size diagnostic runs at
source `f36dc53cc193a52b347e2bda60b6f1c57c2ce71d`. Every invocation ended at
normal EOF with zero tracked live bytes and balanced allocation/deallocation
calls. This is selected container storage, not all malloc traffic or OS RSS.
The diagnostic build uses threshold 191, while the release baseline uses 126;
therefore its recursive-dispatch counts are not a matched before/after result.
Instrumentation changes layout and side effects. Do not compare its elapsed
time to a release binary or equate source-copy counts with hardware atomics.

| Workload | Selected allocations | Simultaneous peak bytes | Per-limb owner-copy sites | COW payload bytes |
| :--- | ---: | ---: | ---: | ---: |
| `loop_1000000` | 10,000,025 | 5,088 | 1,999,997 | 11,821,100 |
| `calls_200000` | 4,768,409 | 7,168 | 768,374 | 3,867,724 |
| `array_200000` | 3,800,037 | 23,205,184 | 599,996 | 3,021,096 |
| `fibonacci_1000_x100` | 907,734 | 12,448 | 1,308,799 | 5,225,996 |
| `fibonacci_10000_x10` | 904,984 | 17,704 | 11,757,739 | 47,021,676 |
| `factorial_5000_x5` | 234,218 | 45,452 | 25,004 | 100,016 |
| `multiply_10000d_x1000` | 140,059 | 57,524 | 34,423,999 | 3,996 |
| `sparse_array_200000` | 4,786,967 | 12,548 | 599,966 | 4,620,844 |

The million-iteration loop executes 19,000,015 VM instructions and performs
2,000,003 literal conversions. The 200,000-call case executes 6,800,016
instructions and performs 600,003 literal conversions. These are diagnostic
source-site counts, with full per-category storage/events preserved in JSON.
Large Fibonacci's 11,757,739 per-limb owner copies and 47,021,676 COW payload bytes
motivate separate ownership/aligned-add and storage experiments. They do not
establish how much elapsed time each source operation consumes.

## Release CPU, memory and native counters

CPU sampling uses a separate release executable, a three-second, one-millisecond
Darwin `sample` window, and repeated copies of the checked program in one process
to keep it alive. Repetition counts, program hashes and sampler argv/status are
recorded; every repeated output is checked. These warm persistent-VM samples
are attribution evidence, not end-to-end timing statistics. Reported leaf
samples below are from the mutually exclusive **top-of-stack collapsed** section;
recursive stack totals are never summed as percentages.

Long Fibonacci records 1,809 aligned-add-loop leaf samples out of 2,449 (73.9%).
The original large multiply records 1,293 basic-multiply, 664 aligned-add and
321 subtract leaf samples out of 2,474. Loop and call samples also show substantial
allocator/free and VM-body leaves. Decimal input is heavily attributed to the
VM instruction body containing inlined numeric construction; that label alone
is not evidence of pure dispatch overhead. Frontend definition registration
includes verification and allocation, so it is not a pure parser measurement.

`gd-for-each-assembly.txt` is the complete extracted release `for_each` function.
It contains `ldadd`/`ldaddal` ownership operations in the mutable-limb path.
Constant-base quotient/remainder work is compiled to `umulh`/`msub` sequences,
not a reason to insert an unmeasured handwritten reciprocal or SIMD routine.
Source ownership lifetime and alias tests must still justify borrowing storage.

Memory is collected in a separate single checked process with native `time -l`.
RSS, memory footprint and selected requested bytes have different meanings and
must not be summed or substituted for one another. This host also exposes native
per-process retired-instruction and cycle fields; branch/cache misses were not
collected, and `xctrace` is unavailable in the installed command-line tools.
Native counters include the complete process, not just one arithmetic kernel.

| Workload | Peak RSS bytes | Native retired instructions | Native cycles |
| :--- | ---: | ---: | ---: |
| `loop_1000000` | 2,015,232 | 7,294,678,931 | 1,262,665,535 |
| `calls_200000` | 2,064,384 | 3,203,072,656 | 579,891,084 |
| `fibonacci_10000_x10` | 2,441,216 | 1,044,066,490 | 618,148,079 |
| `multiply_10000d_x1000` | 2,408,448 | 7,517,569,112 | 5,793,332,684 |
| `multiply_dense_10000d_x500` | 2,408,448 | 3,817,540,882 | 2,923,861,147 |
| `divide_multilimb_900d_450d_s100_x1000` | 2,097,152 | 102,234,733 | 40,975,907 |
| `frontend_definitions_2000` | 2,490,368 | 263,930,317 | 56,952,074 |
| `decimal_input_4000d_x100` | 2,424,832 | 1,334,363,449 | 596,685,178 |
| `array_200000` | 27,803,648 | 2,924,545,594 | 636,234,716 |
| `recursion_50_x1000` | 2,244,608 | 540,471,894 | 106,645,916 |
| `sparse_array_200000` | 2,080,768 | 3,249,720,398 | 614,032,878 |

Raw sample reports are retained as deterministic gzip files with compressed and
uncompressed hashes. Raw `time -l` reports remain text. The initial collector
source is archived because optional native-counter field parsing was added after
that run; the manifest identifies the parser hash and original collector hash.
`profile-bc.py` now reads those native fields directly and records unavailable
adapters explicitly. The supplementary array/recursion run uses that version. Archived collector
source is historical evidence; use the maintained tool for replay. Its current
timeout cleanup stops the POSIX wrapper process group as well as its bc child.
The dense-array peak is about 27.8 MB RSS versus 23.2 MB selected storage in the
diagnostic build; the differing scopes/configurations explain why they need not
match. Sparse-array peak RSS is about 2.1 MB for its distinct sparse workload.

The release binary's `__text` section is 227,884 bytes and `__TEXT` segment is
278,528 bytes (`gd-code-size.txt`). The 4 GiB `__PAGEZERO` reservation is virtual
zero-fill, not resident memory or executable code size. Compiler/library/layout
changes must be recorded before comparing later code-size results.

## Replay and next decisions

Use [checked process benchmarks](../bc-benchmarking.md),
[pinned comparator builds](../bc-comparator-builds.md),
[component drivers](../bc-components.md) and
[separate allocation diagnostics](../bc-profiling.md). `profile-bc.py` accepts
`--config`, `--implementation`, `--summary`, a fresh `--output`, and repeated
`--workload` selections; the summary is used only to choose a bounded source
repetition count. Optional CPU/RSS adapters remain platform-specific, with an
explicit unavailable result elsewhere. No profiler result becomes a hard gate.

Measure each ownership, aligned-add, storage, multiplication-workspace, literal,
VM/frame, array, frontend and compiler change independently. Freeze/report
multiplication thresholds when isolating unrelated changes. Retain the existing
small-product, division, sqrt, modulo and unbalanced-product winners. Use separate
training/holdout data and native runner evidence before accepting architecture
or compiler-specific choices; portable release execution remains the fallback.
