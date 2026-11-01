# Inline limbs and a portable COW heap fallback — #202

Up to three coefficient limbs now live inside their value. Small copies are
independent; larger values retain shared heap vectors and detach on mutation.
This removes small-value owner/vector allocations while retaining cheap sharing
for large literals and snapshots. Scale/sign, zero padding and arithmetic rules
remain separate. No pointer tagging, ISA path or privileged VM operation is used.

## Safety and scope

Growth can relocate elements; shrinking retains heap capacity and iterator
location. Reset/assignment and shared-small detachment can demote. Moves leave
the source absent. Fixed buffer views are acquired after normalization/growth
and end before swaps/resizes. Knuth extents are asserted once before its limb
loops. Exact generic cases force inline-to-heap normalization and verify that
zero-rescale products still trim spare slots; whole-limb shifts preserve copies.
Constant-time numeric zero/length assumptions are not introduced.

The combined tree passes 143 bc cases (580,544 assertions), eight CLI/helper
groups including 36 quick cases, five profiling checks and 103 extensions-
disabled ASan/UBSan number/VM cases (396,335 assertions). The isolated ordinary-
allocation test verifies zero allocations for small construction/copy/add/subtract
at all three traits. Heap/COW diagnostic fixtures deliberately exceed capacity.
The small-multiply winner improves; remaining losing observations are visible.
This is an unaccepted storage experiment, not completed #202 work: terminal/last-use
values, capacity reuse and ownership/arena evaluation remain separate.

## Staged source identities

| Stage | Parent | Candidate | Evidence |
| --- | --- | --- | --- |
| Initial storage | `be6980152a6d0519454e7bc3ff03ea78cff0f69d` | `c3a769a212c7be347a92f7fc2d9108005eea75e9` | 2,160 checked full/held-out invocations; rejected as incomplete |
| Fixed views | same | `4f8444ab50ffb0d30c6ce40b36a13cfee499ba51` | bounded four-way check, then nine-pair comparison |
| Identity scaling | same | `aa97e54307a7e37d80a9e38180a33a3e7d2fd589` | bounded five-way check, then nine-pair comparison |
| Combined local experiment (native rejection below) | `73619a3e5dbc3dbddbca929bdac0a212b77b6772` | `5cd225291808a7f4f57570c17e835396ac2a692b` | fresh source builds; 2,160 checked invocations |

The initial source predates the separately merged reverse-comparison and
discarded-square fixes. Isolated tables keep those identities rather than
relabeling them as combined timings. The combined local pair includes both
fixes in its parent and candidate. Every immutable recipe uses fixed threshold
126, identical compiler/library identities, extensions ON, Release `-O3
-DNDEBUG`, thin LTO and C++20. The host is Apple M1/macOS 27.2, Apple Clang 21
and libc++ header 220106. Native pinned Howard 7.0.3 at c8dabac is the competitive
reference. Actual compiler commands, binary hashes and corpus hashes are retained.
Path-dependent command hashes differ. No instrumented binary enters timing.

Each main experiment has one checked warm-up and nine paired measured rounds
per workload, seeded interleaving and 2,000 paired bootstrap resamples. Full seed
197200 and held-out shapes/seed 197201 remain separate; no threshold or PGO
training was fitted to the held-out inputs. Earlier staged validation also used
held-out shapes to check general regressions; this is not a claim of blinded
candidate selection. The two bounded checks have only three pairs and are
engineering diagnostics, not final precision claims. All source-revision labels
and initial losing data remain available. No aggregate hides a class.

No local builds/profiling ran during timed experiments. This shared workstation
has no CPU/frequency/thermal isolation. Paired intervals do not establish another
host or OS speedup; A/A calibration warns against treating tiny excursions as
universal effects. Native #216 jobs supply separate advisory platform trends.

## Why the first prototype was not adopted

The initial allocation savings coexisted with large fallback regressions:
full true-division ratios were 1.419/1.156, modulo 1.290, square roots
1.108–1.159, sparse/unbalanced products about 1.094. Held-out shapes reproduced
the bad division/modulo/sqrt signals. All original rows and intervals are in
`initial-*-summary.json` and raw CSV. Native division retired instructions grew
from 101,936,709 to 188,238,401 despite lower allocation traffic.

Fixed buffer views move repeated storage-mode/bounds dispatch out of stable
limb loops. Exact identity scaling avoids traversing zero decimal shifts and
whole-limb shifts again, while retaining normalization. The isolated four-way
full/held-out experiment records both revisions, including neutral/losing cases.
Identity scaling particularly helps factorial and whole-limb decimal scaling;
partial-limb scales remain distinct. The final current-base comparison follows.

## Native rejection of the combined variant revision

The local results below do not establish acceptance. Native workflow run
41254982468 compared parent `73619a3` with candidate `5cd2252` and passed its
correctness checks, but its advisory performance reports confirmed regressions.
Linux small square root was 1.076 [1.071, 1.077] and sparse multiplication
1.078 [1.075, 1.081]; macOS small square root was 1.105 [1.088, 1.108]; Windows
sparse multiplication was 1.080 [1.067, 1.088]. Each reproduced in the bounded
confirmation run, while no A/A class confirmed a regression. PR #314 therefore
remains draft. The full host metadata, source identities, decisions and artifact
SHA-256 values are retained in `native9-audit.json`. Original raw artifacts are
attached to that workflow run; local verified ZIP copies are also preserved.

An explicit-union discriminator was rejected after Release output validation
failed with Thin LTO/-O3, despite passing Debug, sanitized and profiling checks.
The cause is unresolved; commit `14849e2` restores the variant representation.
A two-alternative variant is being evaluated separately.
Passing correctness or an advisory workflow alone does not accept its timing.

## Combined variant revision: local paired results

Full loop/calls/dense-array ratios are 0.572/0.658/0.568. Short/long Fibonacci
ratios are 0.739/0.843, and the 100-digit product is 0.821. Held-out shapes
confirm the broad pattern. Small square root is about 3% slower; the original
unbalanced shape is about 3.5% slower with a wide lower interval. These remain
visible, not described as no regression. Major initial kernel regressions are
removed. Several competitive targets remain unmet; math #217 is pending.

### Full

| Workload | Candidate / current parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 0.984 [0.942, 1.034] | 1.045 [0.971, 1.123] |
| `loop_1000000` | 0.572 [0.570, 0.579] | 1.173 [1.166, 1.180] |
| `calls_200000` | 0.658 [0.657, 0.664] | 1.604 [1.597, 1.615] |
| `array_200000` | 0.568 [0.547, 0.574] | 1.208 [1.198, 1.228] |
| `compare_high_1000d_x100000` | 0.541 [0.531, 0.560] | 1.154 [1.127, 1.214] |
| `compare_low_1000d_x100000` | 0.679 [0.675, 0.684] | 1.404 [1.391, 1.428] |
| `compare_equal_1000d_x100000` | 0.682 [0.671, 0.695] | 1.408 [1.334, 1.419] |
| `fibonacci_1000_x100` | 0.739 [0.734, 0.757] | 0.842 [0.839, 0.846] |
| `fibonacci_10000_x10` | 0.843 [0.840, 0.852] | 1.011 [1.008, 1.019] |
| `factorial_1000_x50` | 0.658 [0.653, 0.667] | 1.080 [1.072, 1.091] |
| `factorial_5000_x5` | 0.627 [0.622, 0.627] | 1.041 [1.037, 1.053] |
| `multiply_100d_x100000` | 0.821 [0.819, 0.824] | 0.751 [0.741, 0.760] |
| `multiply_1000d_x10000` | 0.985 [0.981, 0.988] | 2.057 [2.037, 2.058] |
| `multiply_10000d_x1000` | 0.999 [0.995, 1.002] | 7.169 [7.093, 7.187] |
| `divide_scale100_x100000` | 0.574 [0.571, 0.584] | 0.805 [0.799, 0.813] |
| `divide_scale1000_x10000` | 0.828 [0.806, 0.834] | 0.724 [0.697, 0.741] |
| `divide_scale5000_x5000` | 0.949 [0.923, 0.954] | 0.681 [0.675, 0.690] |
| `sqrt_scale100_x5000` | 1.030 [1.026, 1.041] | 0.643 [0.627, 0.645] |
| `sqrt_scale1000_x50` | 1.000 [0.991, 1.003] | 0.905 [0.899, 0.912] |
| `sqrt_scale5000_x5` | 1.006 [1.003, 1.010] | 0.855 [0.852, 0.860] |
| `multiply_dense_1000d_x5000` | 0.985 [0.983, 0.995] | 1.015 [0.994, 1.025] |
| `multiply_dense_10000d_x500` | 0.997 [0.994, 0.999] | 0.935 [0.933, 0.937] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.035 [0.950, 1.042] | 0.805 [0.737, 0.816] |
| `multiply_sparse_10000d_x1000` | 1.012 [1.010, 1.015] | 8.003 [7.948, 8.088] |
| `divide_multilimb_900d_450d_s100_x1000` | 0.958 [0.945, 1.045] | 0.879 [0.865, 0.906] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.988 [0.977, 1.008] | 0.967 [0.933, 0.980] |
| `modulo_1000d_350d_x3000` | 0.972 [0.964, 0.982] | 0.722 [0.718, 0.730] |
| `power_100d_e17_x1000` | 0.960 [0.956, 0.963] | 0.814 [0.801, 0.816] |
| `power_20d_e32_x2000` | 0.885 [0.841, 0.893] | 0.711 [0.702, 0.728] |
| `ibase_literal_flip_20000` | 0.655 [0.644, 0.674] | 1.102 [1.083, 1.120] |
| `recursion_50_x1000` | 0.776 [0.754, 0.782] | 1.844 [1.819, 1.867] |
| `sparse_array_200000` | 0.523 [0.521, 0.525] | 1.023 [1.012, 1.027] |
| `frontend_definitions_2000` | 1.000 [0.997, 1.006] | 3.490 [3.281, 3.550] |
| `decimal_input_4000d_x100` | 1.000 [0.997, 1.004] | 32.506 [31.743, 35.866] |
| `decimal_output_200d_x1000` | 0.972 [0.951, 0.985] | 2.151 [2.082, 2.165] |
| `streaming_statements_20000` | 0.972 [0.971, 0.987] | 3.055 [2.988, 3.091] |

### Holdout

| Workload | Candidate / current parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `startup` | 0.987 [0.888, 1.213] | 1.063 [1.031, 1.137] |
| `loop_875000` | 0.570 [0.564, 0.572] | 1.158 [1.154, 1.164] |
| `calls_175000` | 0.662 [0.661, 0.667] | 1.603 [1.599, 1.609] |
| `array_175000` | 0.564 [0.559, 0.574] | 1.204 [1.183, 1.217] |
| `compare_high_1125d_x87500` | 0.536 [0.528, 0.551] | 1.140 [1.120, 1.154] |
| `compare_low_1125d_x87500` | 0.699 [0.695, 0.701] | 1.414 [1.397, 1.427] |
| `compare_equal_1125d_x87500` | 0.692 [0.685, 0.707] | 1.403 [1.384, 1.417] |
| `fibonacci_1125_x88` | 0.740 [0.737, 0.741] | 0.842 [0.834, 0.845] |
| `fibonacci_11250_x9` | 0.852 [0.844, 0.856] | 1.030 [1.010, 1.034] |
| `factorial_1125_x44` | 0.655 [0.653, 0.657] | 1.084 [1.079, 1.091] |
| `factorial_5625_x5` | 0.624 [0.620, 0.628] | 1.041 [1.039, 1.052] |
| `multiply_112d_x87500` | 0.823 [0.818, 0.844] | 0.717 [0.711, 0.738] |
| `multiply_1125d_x8750` | 0.984 [0.982, 0.986] | 2.319 [2.307, 2.330] |
| `multiply_11250d_x875` | 0.998 [0.995, 0.999] | 7.836 [7.792, 7.871] |
| `divide_scale112_x87500` | 0.580 [0.579, 0.582] | 0.784 [0.778, 0.786] |
| `divide_scale1125_x8750` | 0.684 [0.662, 0.704] | 0.581 [0.569, 0.592] |
| `divide_scale5625_x4375` | 0.681 [0.675, 0.691] | 0.484 [0.481, 0.494] |
| `sqrt_scale112_x4375` | 1.022 [1.019, 1.027] | 0.668 [0.667, 0.674] |
| `sqrt_scale1125_x44` | 1.006 [0.999, 1.022] | 0.893 [0.893, 0.910] |
| `sqrt_scale5625_x5` | 1.006 [1.004, 1.011] | 0.849 [0.841, 0.850] |
| `multiply_dense_1125d_x4375` | 0.987 [0.985, 0.990] | 1.048 [1.042, 1.051] |
| `multiply_dense_11250d_x438` | 0.999 [0.997, 1.037] | 0.972 [0.966, 0.975] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.945 [0.940, 0.948] | 0.651 [0.645, 0.654] |
| `multiply_sparse_11250d_x875` | 1.012 [1.005, 1.020] | 8.568 [8.447, 8.607] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.007 [0.923, 1.119] | 0.687 [0.671, 0.729] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.002 [0.978, 1.012] | 1.277 [1.236, 1.286] |
| `modulo_1125d_393d_x2625` | 1.020 [0.989, 1.044] | 0.569 [0.555, 0.614] |
| `power_112d_e17_x875` | 0.964 [0.957, 0.969] | 0.846 [0.842, 0.847] |
| `power_22d_e32_x1750` | 0.901 [0.878, 0.911] | 0.752 [0.746, 0.760] |
| `ibase_literal_flip_17500` | 0.666 [0.652, 0.693] | 1.117 [1.092, 1.152] |
| `recursion_56_x875` | 0.785 [0.778, 0.799] | 1.859 [1.834, 1.901] |
| `sparse_array_175000` | 0.521 [0.514, 0.525] | 1.018 [1.010, 1.019] |
| `frontend_definitions_1750` | 1.013 [0.973, 1.026] | 3.355 [3.298, 3.458] |
| `decimal_input_4500d_x88` | 0.998 [0.997, 1.001] | 36.892 [36.097, 39.146] |
| `decimal_output_225d_x875` | 0.999 [0.991, 1.060] | 2.198 [2.065, 2.430] |
| `streaming_statements_17500` | 0.974 [0.955, 0.982] | 3.027 [2.988, 3.105] |

## Allocation and representation costs

All 72 final counter reports pass exact outputs and EOF allocation balance.
They are instrumented diagnostics; elapsed times are discarded. The parent
profile source e7c1a83 has the identical Git tree to parent merge 73619a3; the
recorded tree hash proves that equivalence. Earlier view/identity counter runs
also assert exact equality with all 36 initial storage reports, confirming that
loop refinements retain their allocation savings.

| Workload | Parent calls / candidate calls | Parent peak / candidate peak selected bytes |
| --- | --- | --- |
| `loop_1000000` | 10,000,025 / 21 | 5,088 / 5,040 |
| `calls_200000` | 4,768,409 / 1,200,029 | 7,168 / 7,288 |
| `array_200000` | 3,800,037 / 200,030 | 23,205,184 / 14,405,088 |
| `fibonacci_10000_x10` | 904,984 / 202,147 | 17,704 / 18,092 |

The native size probe records Number growing from 24 to 32 bytes. Inline bytes
belong to containing objects; tracked frame/array allocations include element
sizes, while stack and other untracked object storage do not enter selected
allocator counters. These are requested selected bytes, not process malloc or
RSS. Larger values therefore remain a real cost even when allocations fall.

## Native uninstrumented memory/code diagnostics

These are separately checked observations, not timing samples. CPU sampling
repeats programs for attribution; RSS/counters use one checked invocation.
The first sandbox attempt could not read the native clock-rate interface and
is not a measurement. Fresh authorized runs retain per-field native availability.
No branch/cache-miss counter is claimed.

| Workload | Parent peak RSS bytes | Candidate peak RSS bytes | Parent / candidate retired instructions |
| --- | --- | --- | --- |
| `array_200000` | 27,803,648 | 18,153,472 | 2,923,227,566 / 1,825,225,564 |
| `divide_multilimb_900d_450d_s100_x1000` | 2,129,920 | 2,113,536 | 100,838,033 / 80,080,786 |

| Mach-O field | Parent bytes | Candidate bytes |
| --- | --- | --- |
| `__text` | 226,276 | 239,196 |
| `__TEXT` | 278,528 | 294,912 |

`__PAGEZERO` is virtual address reservation, not resident memory. Raw size
outputs, counter collectors, source/command identities, all stage CSV/manifests/
summaries and compressed native samples/counter reports are retained here.
Load compressed JSON with `json.load(gzip.open(path, "rt"))`. This evidence
supports the storage choice on the recorded host; it does not close remaining
#202 ownership/lifetime stages or #197 competitive goals.


## Two-alternative variant experiment (`2b942f5`)

This revision encodes absence as an inline size sentinel, retaining standard
variant lifetime management. It passes the reduced Thin LTO check, 146 Release
bc cases, 106 extensions-disabled ASan/UBSan number/VM cases, five profiling
checks, eight CLI groups and 36 quick corpus cases. Its allocation/COW/live-byte
reports are exactly equal to the previous variant in all 36 full workloads.

Fresh immutable parent/candidate/terminal-consumption/Howard experiments pass
1,440 invocations each for full and held-out shapes. The raw tables retain all
four sources; `gd-two-state` is this candidate. Full loop/call/array ratios are
0.563/0.649/0.554, small multiply 0.803, small square root 1.027 [1.025, 1.030]
and sparse multiplication 1.006 [0.998, 1.011]. Each binary and corpus identity
is in the `two-state-*-manifest.json` files. No local build/test/profile overlaps
these timings. These trials informed subsequent candidate selection and are
not blinded validation.

Native Linux run 41258484932 confirms worse square-root 1.115 [1.099, 1.122]
and sparse-product 1.122 [1.120, 1.125] ratios. Its A/A controls pass. The
verified source artifact SHA-256 is
`f7aabc9e3832be6f28c7424ea74fbfb9e8cd03982132a4c2a82ee3900b5b0f16`.
This revision is therefore also unaccepted; native13 Linux summaries and
orchestration are retained. Later heap-tag access work must be measured afresh.
