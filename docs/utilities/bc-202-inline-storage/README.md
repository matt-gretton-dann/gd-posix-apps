# Inline coefficients and bounded numeric ownership — #202

The final candidate uses an always-live three-limb inline buffer and shared
heap vector owner. A non-null owner selects heap data directly. Small copies
are independent; large immutable values/snapshots retain COW. Growth initializes
all newly exposed elements; copy/move reads only the live inline prefix. Heap
shrinking retains capacity and iterator location. Reset/moves leave an absent
source without clearing or reading unused slots. No union, pointer tagging,
shared_ptr layout or ISA assumption is introduced. The heap path is generic.

Divisor coefficients and Newton guesses are moved only at proven lexical last
use. Final sqrt rescaling consequently avoids one needless COW clone per call.
VM parameter/return consumption was independently merged in PR #315. No general
arithmetic operand is consumed across potentially live control-flow edges.

## Sources and correctness

The isolated final comparison uses workspace baseline
`5e23d646393051f3e22a957495fab69473a01f64` and storage candidate
`58075b41a293a7d3f4cfbad308ff75997702e496`. Both are independently built from
immutable sources with identical fixed threshold 126, Release flags, compiler
and library. The source/compiler commands and binary/corpus hashes are recorded.
The later main merge `b8c2e79` adds workflow/evidence files only to that workspace
source; its production C++/CMake diff against 5e23d64 is empty. Later integration
of those files into the storage topic also leaves production code unchanged.
These identities are retained rather than relabeling old observations.

The combined tree passes 156 Release bc cases (582,397 assertions), 116
extensions-disabled ASan/UBSan number/VM cases (398,188 assertions), five profile
checks and eight CLI/helper groups. Coverage includes every absent/inline/heap
copy/move assignment pairing, self/aliased moves, promotion, COW snapshots,
shrink/demotion, newly exposed elements after copy/shrink/reset, fractional
scale/sign, recursive frames and exact large square roots. The ordinary global
allocation probe independently confirms zero allocations for small numeric
construction/copy/add/subtract at all three limb widths.

The immutable final quick corpus passes 108 checks. Full and held-out corpora
each pass 1,080 output/status/deadline checks: 36 workloads, three implementations
and one warm-up plus nine measured paired rounds. The third implementation is
pinned native Howard 7.0.3. Full seed is 197200; held-out shapes/seed are 197201.
All builds/tests/profiling finish before timing. Paired intervals use 2,000
bootstrap resamples. Apple M1/macOS 27.2, Apple Clang 21 and libc++ header 220106
are this local stratum; there is no CPU/frequency/thermal isolation. Earlier
staged held-out runs informed engineering decisions, so this is not blinded
candidate selection. Native platform replication is separate, not pooled.

## Final storage versus workspace results

Full loop/call/array ratios are 0.649/0.720/0.631. Short/long Fibonacci are
0.746/0.846; the small product is 0.805. Large products are near neutral against
the already improved workspace. Full small sqrt is 0.967. All losing observations
remain visible: held-out unbalanced multiplication is 1.051 [1.047, 1.054],
crossing the provisional 1.05 threshold and therefore inconclusive, not a
no-regression claim. Most other classes are neutral or faster; competitive
Howard gaps remain and belong to subsequent VM/algorithm work. POSIX math is
explicitly pending #217. There is no aggregate performance claim.

| Corpus | Workload | Candidate / workspace [paired 95%] | Candidate / Howard medians |
| --- | --- | --- | --- |
| full | `startup` | 0.985 [0.962, 1.023] | 1.062 |
| full | `loop_1000000` | 0.649 [0.647, 0.663] | 1.297 |
| full | `calls_200000` | 0.720 [0.718, 0.728] | 1.739 |
| full | `array_200000` | 0.631 [0.626, 0.649] | 1.342 |
| full | `compare_high_1000d_x100000` | 0.613 [0.601, 0.618] | 1.299 |
| full | `compare_low_1000d_x100000` | 0.671 [0.654, 0.689] | 1.354 |
| full | `compare_equal_1000d_x100000` | 0.678 [0.653, 0.690] | 1.371 |
| full | `fibonacci_1000_x100` | 0.746 [0.740, 0.753] | 0.845 |
| full | `fibonacci_10000_x10` | 0.846 [0.845, 0.858] | 1.009 |
| full | `factorial_1000_x50` | 0.533 [0.533, 0.538] | 0.877 |
| full | `factorial_5000_x5` | 0.449 [0.449, 0.451] | 0.747 |
| full | `multiply_100d_x100000` | 0.805 [0.804, 0.808] | 0.733 |
| full | `multiply_1000d_x10000` | 0.983 [0.982, 0.992] | 2.051 |
| full | `multiply_10000d_x1000` | 0.997 [0.994, 0.999] | 6.211 |
| full | `divide_scale100_x100000` | 0.673 [0.667, 0.678] | 0.940 |
| full | `divide_scale1000_x10000` | 0.802 [0.786, 0.839] | 0.688 |
| full | `divide_scale5000_x5000` | 0.859 [0.854, 0.867] | 0.617 |
| full | `sqrt_scale100_x5000` | 0.967 [0.959, 0.983] | 0.603 |
| full | `sqrt_scale1000_x50` | 1.000 [0.996, 1.007] | 0.904 |
| full | `sqrt_scale5000_x5` | 1.004 [1.002, 1.006] | 0.852 |
| full | `multiply_dense_1000d_x5000` | 0.982 [0.979, 0.986] | 1.014 |
| full | `multiply_dense_10000d_x500` | 0.994 [0.993, 0.998] | 0.810 |
| full | `multiply_unbalanced_10000d_100d_x2000` | 0.987 [0.938, 0.991] | 0.773 |
| full | `multiply_sparse_10000d_x1000` | 0.982 [0.976, 0.983] | 2.629 |
| full | `divide_multilimb_900d_450d_s100_x1000` | 0.928 [0.901, 0.977] | 0.864 |
| full | `divide_multilimb_1000d_900d_s1000_x500` | 0.971 [0.945, 0.986] | 0.945 |
| full | `modulo_1000d_350d_x3000` | 0.959 [0.941, 0.978] | 0.710 |
| full | `power_100d_e17_x1000` | 0.955 [0.951, 0.961] | 0.810 |
| full | `power_20d_e32_x2000` | 0.871 [0.835, 0.887] | 0.699 |
| full | `ibase_literal_flip_20000` | 0.727 [0.711, 0.734] | 1.200 |
| full | `recursion_50_x1000` | 0.842 [0.831, 0.860] | 2.008 |
| full | `sparse_array_200000` | 0.596 [0.585, 0.601] | 1.146 |
| full | `frontend_definitions_2000` | 1.002 [0.996, 1.012] | 3.477 |
| full | `decimal_input_4000d_x100` | 1.003 [0.988, 1.004] | 33.236 |
| full | `decimal_output_200d_x1000` | 0.988 [0.975, 0.999] | 2.146 |
| full | `streaming_statements_20000` | 0.967 [0.963, 0.974] | 2.923 |
| holdout | `startup` | 0.999 [0.896, 1.140] | 1.041 |
| holdout | `loop_875000` | 0.646 [0.635, 0.658] | 1.300 |
| holdout | `calls_175000` | 0.726 [0.716, 0.727] | 1.728 |
| holdout | `array_175000` | 0.635 [0.634, 0.644] | 1.359 |
| holdout | `compare_high_1125d_x87500` | 0.597 [0.572, 0.625] | 1.254 |
| holdout | `compare_low_1125d_x87500` | 0.674 [0.658, 0.703] | 1.370 |
| holdout | `compare_equal_1125d_x87500` | 0.676 [0.669, 0.684] | 1.354 |
| holdout | `fibonacci_1125_x88` | 0.752 [0.744, 0.761] | 0.839 |
| holdout | `fibonacci_11250_x9` | 0.854 [0.846, 0.858] | 1.026 |
| holdout | `factorial_1125_x44` | 0.525 [0.523, 0.525] | 0.863 |
| holdout | `factorial_5625_x5` | 0.446 [0.443, 0.448] | 0.745 |
| holdout | `multiply_112d_x87500` | 0.856 [0.852, 0.858] | 0.734 |
| holdout | `multiply_1125d_x8750` | 0.986 [0.985, 0.988] | 2.312 |
| holdout | `multiply_11250d_x875` | 0.996 [0.982, 0.999] | 6.928 |
| holdout | `divide_scale112_x87500` | 0.620 [0.617, 0.625] | 0.832 |
| holdout | `divide_scale1125_x8750` | 0.689 [0.668, 0.715] | 0.590 |
| holdout | `divide_scale5625_x4375` | 0.686 [0.650, 0.705] | 0.488 |
| holdout | `sqrt_scale112_x4375` | 0.966 [0.958, 0.976] | 0.634 |
| holdout | `sqrt_scale1125_x44` | 0.999 [0.995, 1.011] | 0.893 |
| holdout | `sqrt_scale5625_x5` | 1.005 [1.004, 1.008] | 0.849 |
| holdout | `multiply_dense_1125d_x4375` | 0.986 [0.983, 0.990] | 1.046 |
| holdout | `multiply_dense_11250d_x438` | 0.995 [0.993, 1.006] | 0.859 |
| holdout | `multiply_unbalanced_11250d_112d_x1750` | 1.051 [1.047, 1.054] | 0.719 |
| holdout | `multiply_sparse_11250d_x875` | 0.984 [0.981, 0.987] | 2.869 |
| holdout | `divide_multilimb_1012d_506d_s112_x875` | 0.827 [0.816, 0.876] | 0.611 |
| holdout | `divide_multilimb_1125d_1012d_s1125_x438` | 0.986 [0.952, 0.995] | 1.256 |
| holdout | `modulo_1125d_393d_x2625` | 0.962 [0.884, 0.972] | 0.537 |
| holdout | `power_112d_e17_x875` | 0.967 [0.958, 0.972] | 0.845 |
| holdout | `power_22d_e32_x1750` | 0.893 [0.882, 0.905] | 0.744 |
| holdout | `ibase_literal_flip_17500` | 0.739 [0.727, 0.774] | 1.233 |
| holdout | `recursion_56_x875` | 0.845 [0.841, 0.853] | 1.982 |
| holdout | `sparse_array_175000` | 0.602 [0.595, 0.606] | 1.152 |
| holdout | `frontend_definitions_1750` | 1.004 [0.986, 1.009] | 3.308 |
| holdout | `decimal_input_4500d_x88` | 1.001 [0.998, 1.006] | 36.697 |
| holdout | `decimal_output_225d_x875` | 0.995 [0.962, 1.024] | 2.136 |
| holdout | `streaming_statements_17500` | 0.971 [0.964, 0.981] | 2.972 |


## Memory and ownership costs

Default Number is 24 bytes in the legacy parent, 32 in the rejected variant
prototype and 40 with separate inline/owner storage on this 64-bit host. This
is a deliberate object-space tradeoff, not a universally size-neutral change.
The array workload's selected allocation calls fall from 3,800,037 in legacy
to 200,030. Requested peak bytes are 23,205,184 legacy, 14,405,088 variant and
16,005,088 separate-owner. The latter costs more requested node storage than
the variant. Fresh checked Darwin array diagnostics nevertheless record RSS
27,754,496 -> 18,153,472 bytes legacy/separate-owner, and retired instructions
2,926,190,387 -> 1,843,704,318. Allocator size classes can absorb layout changes;
this observation does not promise the same usable-size/RSS behavior elsewhere.
Raw native diagnostics and size-probe commands are archived. Selected requested
allocation bytes are not whole-process malloc traffic or RSS.

All 72 exact workspace-parent/candidate profiles balance selected allocations
and deallocations and have zero selected live bytes at EOF. Those reports retain
all categories and copy/COW/move events. Against the prior variant source, the
numeric last-use stage removes 5,000 COW clones in the 5,000-call small-sqrt
workload (95,000 -> 90,000) and halves owned copies (190,001 -> 95,001).

## Rejected and intermediate stages

`staged-variant-report.md` preserves the earlier complete staged narrative and
all original raw full/held-out data remain. Do not treat its local acceptance
wording as final adoption: native validation rejected those candidates.

- The first variant source c3a769a reduced small allocations but regressed true
  division/modulo/sqrt. Fixed buffer views and identity scaling were independently
  measured in 4f8444a and aa97e54. Combined source 5cd2252 still confirmed native
  small-sqrt/sparse-product regressions, so it was not merged.
- The explicit union 2c95f93 was rejected after Thin-LTO/-O3 literal output became
  zero; it passed Debug/sanitized/instrumented checks but failed uninstrumented
  Release validation. 14849e2 reverted it. The cause is unresolved; no global
  compiler flag workaround is adopted. Assignment transition tests were kept.
- Two-alternative variant 2b942f5, owner-tag 364fc61 and borrowed views/ownership
  query 14f381a preserve small-value gains but still confirm Linux sqrt/sparse
  losses. All collected source labels, intervals and diagnostics are retained.
- Numeric last-use 13c2839 improves ownership counts but still confirms legacy
  Linux sqrt/sparse losses. Its 160-check bounded smoke run is diagnostic, not
  final precision evidence; no redundant full acceptance run was claimed.
- Separate-owner 33d59ec clears the native Linux sqrt signal (1.025, within the
  provisional band) but still confirms legacy sparse multiplication (1.076).
  Its 192-check smoke and native22 data are retained; it was not accepted alone.
- Combining storage with workspace source 6834454 passes native24: 950 checks
  per OS and no confirmed paired/A/A regression. That pre-retarget run compares
  against main 3682edf, so it measures combined gains, not isolated storage.
  Exact workspace-only local evidence above is kept separate. Current-main
  PR validation is still required before final merge.

The immutable native ZIPs and `native-artifact-index.json` retain all downloaded
platforms for runs 9/13/15/17/20/22/24, including losing first/confirmation batches,
A/A controls, exact programs/oracles, metadata, logs and decisions. Missing OS
ZIPs in an intermediate canceled/partial run are unavailable, not passes.

#202 remains open: control-flow-aware general liveness, result/capacity reuse,
ownership-policy comparisons and arena evaluation remain separate work. This
report supports only the measured inline/heap representation and lexical
consumption stage. Per-operation multiplication scratch is independently #203.
