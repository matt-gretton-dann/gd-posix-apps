# Cold diagnostic separation — rejected experiment

Refs #202 and #197. Keep the existing ordinary noreturn definitions. Candidate
`0a46f05138e5399a89b9627553e9c6fc460055f2` marks numeric and VM fatal formatters
cold/noinline for GCC/Clang, noinline for MSVC, and ordinary noreturn elsewhere.
All conditions, diagnostics and runtime semantics remain checked. The exact
patch is retained; the final PR removes these hints from production.

## Measurements and decision

Actual local baseline `7974cb3d8b16c98441f9dc960f6c91f52736b06d` and native
baseline `d472b0065e213419fc6b046d89d6994ab85baf9c` have no non-document changes;
their identities are retained separately, not relabeled. Local measurements use
Apple M1/macOS 27.2, Apple Clang 21/libc++ 220106, Release thin LTO, split 126,
extensions ON. Full and held-out each check 900 invocations: 45 cases, two roles,
one warmup and nine shuffled paired samples. All 1800 output/status/stderr/deadline
checks pass. Schedule seeds 197231/197232 and corpus seeds 197200/197201 are
separate; intervals use 2000 bootstrap resamples. No builds/tests/profiles/RSS
diagnostics overlap speed timings. Host scheduling/frequency/thermals are uncontrolled.

No local class has CI95 wholly above 1.05; timings are broadly neutral. Scalar
loop is 1.000 [0.988,1.010] full and 1.005 [1.001,1.025] held-out. The chain is
1.002 [1.000,1.007] and 1.002 [0.990,1.009]. All intervals remain below, including
losing/inconclusive classes; controls are not subtracted from process timings.

Separate RSS runs check all 270 invocations (three per full case/role). Raw
getrusage maxima are bytes under the retained local reference. Process RSS
includes loader/frontend/code/values/output and is not isolated workspace or
selected allocator payload. Long Fibonacci sampled medians rise from 2736128
to 2981888 bytes; definitions from 2441216 to 2506752, loop 1966080 to 1998848.
No general RSS benefit is shown. Three samples are not portable peak bounds or
confidence intervals, and no allocator/address-layout cause is asserted.

The instruction section shrinks from 260976 to 257536 bytes (3440 bytes, ~1.3%).
Both page-rounded __TEXT segments remain 311296 bytes and executable files
508048 bytes. This layout-only saving provides no demonstrated executable-size,
resident-memory or end-to-end speed benefit. Reject the additional hints rather
than extending a section-size observation into a performance claim.

All fourteen checks at the candidate pass. Digest-verified native artifacts
contain 1700 checked rows each (5100 total), with no confirmed regression under
the policy. Linux small division and sparse array remain inconclusive near 1.05;
macOS scalar/definition intervals are wide and noisy; Windows loop/calls/array
also retain inconclusive intervals. The complete ZIPs and classifications remain.
Release passes 212 cases/691942 assertions, eight CLI/helper groups and six
malformed-offset groups. A standalone syntax probe verifies the generic hint
fallback. No new lifetime implementation is introduced by this compiler experiment.

## #202 completion decisions

| Stage | Accepted implementation or explicit decision |
| --- | --- |
| Scoped allocation/ownership evidence | Profiling counters and exact-source reports under docs/utilities/bc-profiling.md and the #200 baseline |
| Inline coefficients with exact promotion | #314 retains three inline limbs and the generic shared-vector fallback |
| Proven transfers and capacity reuse | #315 parameter/terminal returns; #342 conservative heap arithmetic liveness; #326/#331 reusable result/frame owners |
| Numeric ownership comparison | #344 retains inline+COW after measured unique/deep-copy and zero-inline controls, preserving the unique policy memory tradeoff |
| Scratch/arena lifetime policy | #316/#203 bounded lexical multiplication scratch; #344 rejects cross-item/activation arena reset because reachable values escape |
| Cold diagnostics | This exact-source experiment is rejected for lack of demonstrated end-to-end benefit |

These complete the bounded #202 policy choices. Typed/compact execution, fusion
and dispatch alternatives remain #207; parser buffering remains #210. No cross-
frame alias or unbounded numeric arena is introduced. The overall #197 competitive
target and fresh final coverage remain open; this report does not claim them.

## full paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.055 [0.958,1.196] |
| `loop_1000000` | 1.000 [0.988,1.010] |
| `calls_200000` | 0.995 [0.980,1.023] |
| `array_200000` | 1.000 [0.994,1.017] |
| `compare_high_1000d_x100000` | 1.014 [0.991,1.025] |
| `compare_low_1000d_x100000` | 0.993 [0.989,1.009] |
| `compare_equal_1000d_x100000` | 0.994 [0.982,1.011] |
| `fibonacci_1000_x100` | 1.004 [0.989,1.009] |
| `fibonacci_10000_x10` | 1.001 [0.992,1.005] |
| `factorial_1000_x50` | 1.000 [1.000,1.003] |
| `factorial_5000_x5` | 1.000 [0.997,1.005] |
| `multiply_100d_x100000` | 1.009 [1.004,1.017] |
| `multiply_1000d_x10000` | 1.010 [0.991,1.016] |
| `multiply_10000d_x1000` | 1.007 [0.998,1.015] |
| `divide_scale100_x100000` | 0.993 [0.969,1.032] |
| `divide_scale1000_x10000` | 1.001 [0.955,1.010] |
| `divide_scale5000_x5000` | 0.995 [0.982,1.042] |
| `sqrt_scale100_x5000` | 1.001 [0.994,1.004] |
| `sqrt_scale1000_x50` | 1.002 [0.996,1.008] |
| `sqrt_scale5000_x5` | 0.997 [0.993,1.000] |
| `multiply_dense_1000d_x5000` | 0.993 [0.989,0.996] |
| `multiply_dense_10000d_x500` | 1.001 [0.982,1.015] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.001 [0.983,1.012] |
| `multiply_sparse_10000d_x1000` | 1.004 [0.999,1.006] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.005 [0.948,1.068] |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.033 [0.992,1.044] |
| `modulo_1000d_350d_x3000` | 1.002 [0.983,1.029] |
| `power_100d_e17_x1000` | 0.997 [0.992,1.021] |
| `power_20d_e32_x2000` | 1.006 [0.948,1.034] |
| `ibase_literal_flip_20000` | 1.009 [0.981,1.023] |
| `recursion_50_x1000` | 1.003 [0.979,1.039] |
| `sparse_array_200000` | 0.997 [0.987,1.020] |
| `frontend_definitions_2000` | 1.023 [0.989,1.036] |
| `decimal_input_4000d_x100` | 0.999 [0.981,1.008] |
| `decimal_output_200d_x1000` | 0.970 [0.879,1.006] |
| `streaming_statements_20000` | 1.002 [0.987,1.011] |
| `square_100d_x100000` | 1.000 [0.986,1.018] |
| `square_1000d_x10000` | 1.000 [0.998,1.008] |
| `square_10000d_x500` | 1.000 [0.997,1.002] |
| `literal_repeat_2000d_x2000` | 1.019 [0.952,1.113] |
| `binary_output_1000d_x300` | 1.011 [0.972,1.079] |
| `octal_output_1000d_x300` | 1.006 [0.987,1.020] |
| `binary_input_100000d_x3` | 1.003 [0.998,1.008] |
| `hex_input_100000d_x3` | 1.004 [0.990,1.005] |
| `temporary_chain_1000d_x50000` | 1.002 [1.000,1.007] |

## holdout paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.043 [0.918,1.062] |
| `loop_875000` | 1.005 [1.001,1.025] |
| `calls_175000` | 0.999 [0.995,1.013] |
| `array_175000` | 1.006 [0.981,1.009] |
| `compare_high_1125d_x87500` | 1.010 [0.964,1.024] |
| `compare_low_1125d_x87500` | 1.009 [1.000,1.019] |
| `compare_equal_1125d_x87500` | 1.001 [0.982,1.017] |
| `fibonacci_1125_x88` | 0.989 [0.973,1.018] |
| `fibonacci_11250_x9` | 0.998 [0.991,1.007] |
| `factorial_1125_x44` | 1.002 [0.997,1.007] |
| `factorial_5625_x5` | 1.001 [0.999,1.005] |
| `multiply_112d_x87500` | 1.009 [0.984,1.030] |
| `multiply_1125d_x8750` | 1.002 [0.990,1.012] |
| `multiply_11250d_x875` | 1.007 [1.000,1.011] |
| `divide_scale112_x87500` | 0.989 [0.964,0.995] |
| `divide_scale1125_x8750` | 0.998 [0.952,1.013] |
| `divide_scale5625_x4375` | 0.993 [0.989,1.011] |
| `sqrt_scale112_x4375` | 1.003 [0.998,1.015] |
| `sqrt_scale1125_x44` | 1.001 [0.996,1.013] |
| `sqrt_scale5625_x5` | 1.000 [0.995,1.002] |
| `multiply_dense_1125d_x4375` | 0.991 [0.981,1.007] |
| `multiply_dense_11250d_x438` | 1.000 [0.993,1.007] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.006 [0.999,1.008] |
| `multiply_sparse_11250d_x875` | 1.007 [1.001,1.021] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.019 [0.963,1.070] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.024 [1.001,1.033] |
| `modulo_1125d_393d_x2625` | 1.014 [0.933,1.072] |
| `power_112d_e17_x875` | 0.990 [0.978,0.995] |
| `power_22d_e32_x1750` | 1.011 [0.932,1.046] |
| `ibase_literal_flip_17500` | 1.011 [0.993,1.027] |
| `recursion_56_x875` | 1.006 [0.993,1.014] |
| `sparse_array_175000` | 0.999 [0.988,1.010] |
| `frontend_definitions_1750` | 1.008 [0.987,1.024] |
| `decimal_input_4500d_x88` | 0.997 [0.990,1.010] |
| `decimal_output_225d_x875` | 1.029 [0.992,1.145] |
| `streaming_statements_17500` | 0.992 [0.974,1.006] |
| `square_112d_x87500` | 1.008 [1.002,1.031] |
| `square_1125d_x8750` | 1.026 [1.018,1.033] |
| `square_11250d_x438` | 0.999 [0.988,1.007] |
| `literal_repeat_2250d_x1750` | 1.027 [0.949,1.083] |
| `binary_output_1125d_x263` | 1.026 [0.993,1.057] |
| `octal_output_1125d_x263` | 1.007 [0.991,1.016] |
| `binary_input_112500d_x3` | 1.001 [0.995,1.017] |
| `hex_input_112500d_x3` | 1.003 [0.989,1.017] |
| `temporary_chain_1125d_x43750` | 1.002 [0.990,1.009] |
