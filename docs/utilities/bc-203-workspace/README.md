# Reusable Karatsuba limb workspace — #203

Parent `73619a3e5dbc3dbddbca929bdac0a212b77b6772` and candidate
`dc86aaa51cc1a25214943e7a8c4e078342b8dfed` use independently built immutable
Release binaries, fixed split point 126, Apple Clang 21, libc++ header 220106,
C++20 and thin LTO on Apple M1/macOS 27.2. Actual commands, binary hashes,
compiler identities and every corpus hash are in the manifests. No instrumented
binary enters a timing comparison. Neither source is relabeled after later edits.

## Method and validation

Full and held-out experiments each pass 1,440 checks: 36 workloads, four
implementations, one checked warm-up and nine paired measured rounds. The raw
rows retain parent, workspace candidate, the independently evaluated inline-
storage candidate and pinned native Howard 7.0.3. The tables below select only
workspace/parent; they do not attribute the inline candidate's gains here.
Full seed is 197200; held-out shapes/seed are 197201. Timing order is seeded and
interleaved within rounds; intervals use 2,000 paired bootstrap resamples.
No local build, test or diagnostic overlaps timing. This shared workstation has
no CPU/frequency/thermal isolation, and intervals do not establish other hosts.
The quick corpus also passes all 144 checks before full timing begins.

The implementation passes 140 bc cases (581,807 assertions), 100 extensions-
disabled ASan/UBSan number/VM cases (397,598 assertions), five profiling checks
and eight CLI/helper groups. Independent decimal convolution tests were committed
before the implementation. Exact planned scratch bounds are exercised with
nonzero scratch and guarded output/scratch at all three generic limb widths.

## Process results and limits

Large dense-product ratios are 0.864–0.867 full and 0.883–0.887 held out.
Sparse-product ratios are 0.338 full and 0.346 held out. Small products,
factorial, square root, loop and call classes remain mostly neutral. These are
class results, not an aggregate. Several competitive targets remain unmet;
the Howard column makes the remaining gap visible. It is a ratio of medians,
not a separate paired interval. POSIX math coverage is pending #217.

| Corpus | Workload | Candidate / parent [paired 95%] | Candidate / Howard medians |
| --- | --- | --- | --- |
| full | `startup` | 0.992 [0.948, 1.012] | 1.038 |
| full | `loop_1000000` | 0.999 [0.975, 1.040] | 2.035 |
| full | `calls_200000` | 1.001 [0.989, 1.006] | 2.417 |
| full | `array_200000` | 1.013 [0.965, 1.025] | 2.148 |
| full | `compare_high_1000d_x100000` | 1.005 [0.993, 1.013] | 2.159 |
| full | `compare_low_1000d_x100000` | 0.993 [0.978, 1.006] | 2.059 |
| full | `compare_equal_1000d_x100000` | 1.008 [0.991, 1.015] | 2.089 |
| full | `fibonacci_1000_x100` | 1.000 [0.996, 1.003] | 1.136 |
| full | `fibonacci_10000_x10` | 1.000 [0.999, 1.004] | 1.199 |
| full | `factorial_1000_x50` | 0.999 [0.995, 1.004] | 1.643 |
| full | `factorial_5000_x5` | 1.000 [0.997, 1.001] | 1.666 |
| full | `multiply_100d_x100000` | 1.003 [0.999, 1.015] | 0.918 |
| full | `multiply_1000d_x10000` | 0.998 [0.995, 1.000] | 2.085 |
| full | `multiply_10000d_x1000` | 0.867 [0.831, 0.912] | 6.218 |
| full | `divide_scale100_x100000` | 0.997 [0.992, 1.006] | 1.399 |
| full | `divide_scale1000_x10000` | 1.000 [0.991, 1.057] | 0.867 |
| full | `divide_scale5000_x5000` | 0.999 [0.995, 1.016] | 0.718 |
| full | `sqrt_scale100_x5000` | 0.996 [0.992, 1.014] | 0.619 |
| full | `sqrt_scale1000_x50` | 1.001 [0.992, 1.013] | 0.898 |
| full | `sqrt_scale5000_x5` | 1.000 [0.996, 1.003] | 0.849 |
| full | `multiply_dense_1000d_x5000` | 1.002 [1.001, 1.006] | 1.032 |
| full | `multiply_dense_10000d_x500` | 0.864 [0.860, 0.868] | 0.812 |
| full | `multiply_unbalanced_10000d_100d_x2000` | 0.992 [0.986, 0.994] | 0.771 |
| full | `multiply_sparse_10000d_x1000` | 0.338 [0.336, 0.345] | 2.678 |
| full | `divide_multilimb_900d_450d_s100_x1000` | 0.966 [0.917, 0.999] | 0.889 |
| full | `divide_multilimb_1000d_900d_s1000_x500` | 0.998 [0.989, 1.004] | 0.953 |
| full | `modulo_1000d_350d_x3000` | 0.995 [0.979, 1.002] | 0.736 |
| full | `power_100d_e17_x1000` | 0.998 [0.995, 1.001] | 0.845 |
| full | `power_20d_e32_x2000` | 1.002 [0.996, 1.008] | 0.805 |
| full | `ibase_literal_flip_20000` | 0.993 [0.983, 1.012] | 1.661 |
| full | `recursion_50_x1000` | 1.000 [0.992, 1.002] | 2.375 |
| full | `sparse_array_200000` | 1.000 [0.986, 1.016] | 1.941 |
| full | `frontend_definitions_2000` | 1.003 [0.998, 1.020] | 3.521 |
| full | `decimal_input_4000d_x100` | 1.006 [1.004, 1.010] | 34.583 |
| full | `decimal_output_200d_x1000` | 0.994 [0.972, 1.007] | 2.154 |
| full | `streaming_statements_20000` | 0.997 [0.986, 1.007] | 3.022 |
| holdout | `startup` | 0.960 [0.847, 1.156] | 1.055 |
| holdout | `loop_875000` | 1.001 [0.998, 1.015] | 2.049 |
| holdout | `calls_175000` | 1.001 [0.990, 1.011] | 2.397 |
| holdout | `array_175000` | 1.012 [0.996, 1.018] | 2.149 |
| holdout | `compare_high_1125d_x87500` | 1.005 [0.984, 1.016] | 2.140 |
| holdout | `compare_low_1125d_x87500` | 0.999 [0.981, 1.006] | 2.027 |
| holdout | `compare_equal_1125d_x87500` | 1.008 [0.994, 1.022] | 2.042 |
| holdout | `fibonacci_1125_x88` | 1.002 [0.998, 1.005] | 1.135 |
| holdout | `fibonacci_11250_x9` | 0.999 [0.991, 1.003] | 1.207 |
| holdout | `factorial_1125_x44` | 1.002 [0.998, 1.005] | 1.651 |
| holdout | `factorial_5625_x5` | 1.000 [0.997, 1.007] | 1.675 |
| holdout | `multiply_112d_x87500` | 0.990 [0.989, 1.000] | 0.864 |
| holdout | `multiply_1125d_x8750` | 0.998 [0.981, 1.008] | 2.339 |
| holdout | `multiply_11250d_x875` | 0.883 [0.874, 0.884] | 6.936 |
| holdout | `divide_scale112_x87500` | 1.004 [0.992, 1.006] | 1.360 |
| holdout | `divide_scale1125_x8750` | 1.010 [0.992, 1.049] | 0.865 |
| holdout | `divide_scale5625_x4375` | 1.006 [1.000, 1.014] | 0.718 |
| holdout | `sqrt_scale112_x4375` | 0.999 [0.995, 1.002] | 0.656 |
| holdout | `sqrt_scale1125_x44` | 1.000 [0.992, 1.016] | 0.887 |
| holdout | `sqrt_scale5625_x5` | 0.999 [0.997, 1.000] | 0.847 |
| holdout | `multiply_dense_1125d_x4375` | 0.998 [0.980, 1.004] | 1.062 |
| holdout | `multiply_dense_11250d_x438` | 0.887 [0.886, 0.889] | 0.863 |
| holdout | `multiply_unbalanced_11250d_112d_x1750` | 0.995 [0.994, 0.998] | 0.684 |
| holdout | `multiply_sparse_11250d_x875` | 0.346 [0.345, 0.350] | 2.923 |
| holdout | `divide_multilimb_1012d_506d_s112_x875` | 0.907 [0.854, 1.000] | 0.614 |
| holdout | `divide_multilimb_1125d_1012d_s1125_x438` | 0.995 [0.979, 1.004] | 1.270 |
| holdout | `modulo_1125d_393d_x2625` | 0.976 [0.940, 1.031] | 0.553 |
| holdout | `power_112d_e17_x875` | 1.000 [0.983, 1.012] | 0.876 |
| holdout | `power_22d_e32_x1750` | 0.999 [0.993, 1.008] | 0.833 |
| holdout | `ibase_literal_flip_17500` | 0.997 [0.971, 1.001] | 1.671 |
| holdout | `recursion_56_x875` | 1.006 [0.998, 1.008] | 2.362 |
| holdout | `sparse_array_175000` | 1.000 [0.994, 1.007] | 1.949 |
| holdout | `frontend_definitions_1750` | 0.996 [0.991, 1.015] | 3.285 |
| holdout | `decimal_input_4500d_x88` | 1.003 [1.001, 1.008] | 36.084 |
| holdout | `decimal_output_225d_x875` | 0.995 [0.970, 1.003] | 2.153 |
| holdout | `streaming_statements_17500` | 1.000 [0.995, 1.007] | 3.030 |

## Allocation and storage evidence

In the original 10,000-digit product workload, selected allocation calls drop
from 410,059 to 11,059 and requested allocated bytes from 165,215,592 to
25,995,592. Selected peak live bytes fall from 57,752 to 55,772. Karatsuba call
count is unchanged at 40,000; the measured source retains the same fixed split
control. All 72 parent/candidate reports balance allocations/deallocations and
show zero selected live bytes at EOF. These are scoped requested allocation
metrics, not whole-process RSS or allocator usable sizes. The parent's profile
source e7c1a83 has the same Git tree as 73619a3, as recorded by #211 evidence.

Number layout is unchanged. Native Mach-O __text is 226,276 -> 226,528 bytes;
__TEXT remains 278,528 bytes. Raw native size reports retain other sections;
the 4 GiB __PAGEZERO virtual reservation is not resident memory.

## Native platform replication

[Run 16](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41259365703)
passes 950 output checks per OS (2,850 total), with no confirmed regression
in paired or A/A reports. Dense 10,000-digit product ratios are 0.895 Linux,
0.870 macOS and 0.892 Windows. Sparse-product ratios are 0.363, 0.340 and
0.345 respectively. Each OS rebuilds exact base/head and an independent A/A
base plus pinned Howard natively; no WSL comparator or stale binary is used.
Artifact hashes, decisions, all class summaries and orchestration are archived.
Original raw samples/build logs remain workflow artifacts; verified local ZIP
copies are retained. Host/compiler/image differences prohibit cross-OS pooling.

This completes the workspace experiment only. Dedicated squaring, blocked/
asymmetric dispatch, repeated threshold sweeps and stable portable defaults
remain separate #203 stages. Scratch is scoped to one operation and never owns
escaping values; no thread cache or cross-frame arena is introduced.

## Integration with the merged lifetime stage

The isolated measurements above retain their original source identities. After
integrating main `3682edf` (PR #315), the combined tree passes 141 bc cases
(581,809 assertions), all eight CLI/helper groups, 14 VM cases under ASan/UBSan
(5,026 assertions) and five profiling checks. The numeric kernel is unchanged.
A test-only structured binding now borrows shape pairs to satisfy GCC's
warnings-as-errors policy. Fresh latest-head native checks compare against the
current base; no earlier timing is relabeled as that integrated source.


## Integrated Windows regression hold (native run 21)

The integrated source `0374daf` passes ordinary CI and native output checks,
but is not accepted for merge. Linux/macOS each pass 950 invocations with no
confirmed performance regression. Windows passes 980 output checks including
one confirmation batch, but confirms multi-limb division at 1.261
[1.192, 1.343] versus base `3682edf`. Small square root is 1.175 with a wide
interval and is inconclusive. Earlier isolated workspace runs 16/18 were
neutral for division. All observations, including this new losing result, are
retained in the native21 summaries/decisions and verified artifact hashes.

An MSVC-only `__declspec(noinline)` boundary around the large coefficient
Algorithm D method is now an experiment to isolate caller inlining choices.
This does not establish the cause of the regression. Other compilers keep their
ordinary policy; arithmetic and generic portability are unchanged. The
[Microsoft declaration documentation](https://learn.microsoft.com/en-us/cpp/cpp/noinline)
explains this compiler control. Fresh native validation must establish its
cost before acceptance; no global optimization flag is weakened.


## MSVC boundary result (native run 23)

Source `5e23d646393051f3e22a957495fab69473a01f64` passes all six build/test
jobs, CodeQL and native run 23. Every OS passes 950 output checks, with no
confirmed regression in paired or A/A reports. The Windows division ratio is
0.983 [0.870, 1.020], clearing the earlier run-21 signal; small sqrt is 0.994
with a wide interval and remains inconclusive. Linux division 1.057
[0.995, 1.071] and macOS division 0.950 [0.727, 1.085] are also inconclusive,
not evidence of a universal no-regression bound. Large-product gains persist.

This supports retaining the targeted MSVC compilation boundary. It does not
prove why the original integration changed performance: the runs use separate
hosted VMs, and the compiler/caller explanation remains a hypothesis. Earlier
losing observations are retained, and this is not an unlimited rerun policy.
The attribute experiment changes code generation only on MSVC-family compilers;
other compilers preserve their normal policy and all arithmetic is unchanged.

Native23 decisions, complete class summaries, paired manifests with compiler/
host metadata, orchestration and verified artifact hashes are archived here.
The measured source identity remains 5e23d64 after this evidence-only update.
The workspace and compilation-boundary stage is ready for green latest-head
merge. Dedicated squares, asymmetric dispatch, tuning and stable defaults still
belong to the remaining #203 work.
