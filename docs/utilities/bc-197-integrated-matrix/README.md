# First matched integration matrix — candidates and native holds

This report preserves complete observations for #200/#197. It does not accept
every candidate or claim that the competitive target has been achieved. Native
bounded confirmations put the initial integrated frame and hybrid-array heads
on hold despite advisory green workflows. Changed compiler-specific candidates
must earn their own native evidence; later sources are never substituted into
these historical measurements.

## Exact revisions

| Stage | Source | Comparison |
| --- | --- | --- |
| Accepted blocked kernel | `00d5896be1768d1936a075ccd47e4b529a914dd2` | Baseline |
| Direct decimal/scalar path and Apple alignment | `00365f2f75324278024e665ba025aa47abbf6d70` | Accepted kernel |
| Reusable activations | `d76e408e069a4824eca7ab1fc3fa35100a14a070` | Decimal stage |
| Verified executor with checked oracle | `357d15a4abd3736b432953b9269ba1057e234f02` | Frame stage |
| Hybrid arrays and borrowed lookup | `91b7b481178bebb6d8a9c660649d59c71527743a` | Verified stage |
| Native Howard 7.0.3 | `c8dabac65d39fd527a9264a5e505a99b1d8df3d8` | Competitive reference |

## Method and limits

Apple M1/macOS 27.2, Apple Clang 21/libc++ header 220106, Release thin LTO,
extensions ON, fixed multiplication split point 126, profile/coverage OFF.
Compiler commands, executable hashes, libraries, host and corpus identities
are in the manifests. All 240 quick, 2400 full and 2400 held-out invocations
pass exact output, status, stderr and deadline checks. Full/held-out use one
checked warm-up and nine seeded paired rounds, with 2000 paired bootstrap
resamples. Each case runs the six implementations serially in seeded order.
No local builds/tests/profiles overlap these timings. Shared-host contention,
frequency and thermal state are not controlled.

The adjacent analysis pairs the same measurement round rather than dividing
confidence intervals or composing independently collected speedups. Startup
remains inconclusive pending calibration. No local adjacent interval is wholly
above the provisional 1.05 material threshold; that is not a neutral-performance
claim. Held-out modulo in the decimal stage is 1.070 [0.992,1.093]. Frame reuse
also has measurable roughly 3–4% loop/Fibonacci costs. Native losses below remain
visible instead of being hidden by a local aggregate or cumulative gain.

## Principal adjacent results

| Stage/workload | Full | Held-out |
| --- | --- | --- |
| Decimal input | 0.143 | 0.128 |
| Frame calls | 0.801 | 0.797 |
| Verified loop | 0.923 | 0.921 |
| Verified calls | 0.922 | 0.922 |
| Hybrid dense arrays | 0.805 | 0.817 |
| Hybrid sparse arrays | 1.008 | 1.011 |

All class intervals are retained below. Conversion components pass all 360
exact result/checksum/status checks; one-digit input is 0.965 [0.941,0.975],
while 9/10/100/1000/10000-digit ratios are approximately 0.512/0.387/0.115/
0.0197/0.00178. These internal intervals include reset/consumer/validation
work and do not subtract controls or claim pure-kernel costs.

## Allocation observations

All 200 separate diagnostic invocations pass exact oracles and balance selected
allocations at EOF. They are not elapsed timings or process RSS. The accepted
counter source is 565564d, whose production C++ matches timing source 00d5896;
every actual counter source and binary hash remains explicit in provenance.

| Workload/stage | Array allocation calls | Allocated bytes | Peak selected bytes |
| --- | ---: | ---: | ---: |
| array_200000 / verified | 200001 | 16000048 | 16000048 |
| array_200000 / arrays | 20 | 25165872 | 18874464 |
| sparse_array_200000 / verified | 32 | 2528 | 2528 |
| sparse_array_200000 / arrays | 32 | 2544 | 2544 |

Dense allocation calls fall dramatically, but geometric vector growth increases
peak selected array payload by about 18% and cumulative requested bytes by about
57%. This is a real tradeoff. Sparse payload differs only by the container header
in the measured case. Paged, flat/sorted and hash alternatives remain #209 work.

## Native decisions

Every ZIP below was checked against its Actions digest and exact candidate
identity. Correctness passes on all OSes. #326's event used main 00d5896 as
base before PR retargeting; its native comparison includes decimal conversion
and frames. #328 and #329 use their actual adjacent predecessors.

| PR / OS | Checked invocations | Confirmed regressions |
| --- | ---: | --- |
| #322 / linux | 1450 | None |
| #322 / macos | 1450 | None |
| #322 / windows | 1450 | None |
| #326 / linux | 1450 | None |
| #326 / macos | 1450 | None |
| #326 / windows | 1510 | compare_low_1000d_x100000 |
| #328 / linux | 1450 | None |
| #328 / macos | 1450 | None |
| #328 / windows | 1450 | None |
| #329 / linux | 1660 | loop_1000000; calls_200000; compare_high_1000d_x100000; compare_low_1000d_x100000; ibase_literal_flip_20000; sparse_array_200000 |
| #329 / macos | 1450 | None |
| #329 / windows | 1450 | None |

Windows #326 confirms low-digit comparison at 1.114 [1.091,1.147]. Linux #329
confirms loop 1.141, calls 1.112, high comparison 1.109, low comparison 1.097,
base-changing literals 1.087 and sparse arrays 1.082. Their raw confirmation
batches and intervals remain in the ZIPs. Neither result proves an inlining
or hardware cause. Both candidates stay on hold while changed candidates are
evaluated. The native workflow is intentionally advisory, so its green result
alone is insufficient for adoption.

## Superseded standalone stages

The standalone frame and verified-executor full/held-out raw data are retained
with their original sources and parent 0868b4a. In particular, standalone frame
held-out division was 1.127 [1.106,1.207]. Integration is an actual code change,
not a favorable replacement of those rows. Standalone verified code retains
both executor instantiations and increases the recorded Mach-O __TEXT segment
from 278528 to 294912 bytes; this segment size is not process RSS.

## Full adjacent ratios

| Workload | Decimal / accepted | Frames / decimal | Verified / frames | Arrays / verified |
| --- | --- | --- | --- | --- |
| `startup` | 1.021 [0.980,1.033] | 1.018 [0.964,1.027] | 0.997 [0.956,1.018] | 0.983 [0.955,1.045] |
| `loop_1000000` | 0.993 [0.979,0.998] | 1.035 [1.001,1.041] | 0.923 [0.912,0.926] | 1.000 [0.997,1.028] |
| `calls_200000` | 0.996 [0.989,1.001] | 0.801 [0.794,0.803] | 0.922 [0.918,0.924] | 1.003 [0.999,1.006] |
| `array_200000` | 0.994 [0.987,1.002] | 1.022 [1.012,1.028] | 0.948 [0.936,0.956] | 0.805 [0.800,0.819] |
| `compare_high_1000d_x100000` | 0.992 [0.966,1.000] | 1.032 [1.011,1.049] | 0.957 [0.938,0.984] | 1.001 [0.985,1.031] |
| `compare_low_1000d_x100000` | 0.992 [0.979,1.005] | 1.020 [1.000,1.050] | 0.959 [0.950,1.005] | 0.977 [0.971,1.014] |
| `compare_equal_1000d_x100000` | 0.990 [0.979,1.019] | 1.019 [0.995,1.043] | 0.958 [0.917,0.998] | 1.005 [0.984,1.034] |
| `fibonacci_1000_x100` | 0.996 [0.994,1.002] | 1.040 [1.032,1.041] | 0.924 [0.923,0.930] | 1.004 [0.998,1.013] |
| `fibonacci_10000_x10` | 1.001 [0.996,1.004] | 1.026 [1.021,1.032] | 0.956 [0.953,0.960] | 1.002 [0.987,1.007] |
| `factorial_1000_x50` | 1.001 [0.992,1.011] | 1.011 [1.004,1.012] | 0.976 [0.974,0.977] | 1.002 [1.000,1.009] |
| `factorial_5000_x5` | 1.001 [0.998,1.025] | 1.000 [0.974,1.004] | 0.994 [0.989,1.008] | 1.004 [0.998,1.007] |
| `multiply_100d_x100000` | 0.993 [0.983,0.998] | 1.027 [1.019,1.031] | 0.961 [0.956,0.979] | 0.995 [0.979,1.004] |
| `multiply_1000d_x10000` | 0.999 [0.986,1.011] | 0.994 [0.990,1.006] | 1.004 [0.995,1.008] | 0.995 [0.990,1.003] |
| `multiply_10000d_x1000` | 0.936 [0.928,0.936] | 1.000 [0.998,1.004] | 0.995 [0.993,1.002] | 1.000 [0.998,1.002] |
| `divide_scale100_x100000` | 0.997 [0.987,1.008] | 1.006 [1.004,1.013] | 0.943 [0.937,0.951] | 1.002 [0.995,1.013] |
| `divide_scale1000_x10000` | 0.996 [0.992,1.004] | 1.002 [1.002,1.009] | 0.996 [0.989,1.004] | 0.999 [0.992,1.031] |
| `divide_scale5000_x5000` | 0.999 [0.998,1.003] | 1.003 [0.998,1.012] | 0.995 [0.986,1.009] | 1.001 [0.987,1.012] |
| `sqrt_scale100_x5000` | 1.004 [1.000,1.009] | 0.998 [0.990,1.005] | 0.993 [0.992,0.999] | 0.996 [0.990,1.006] |
| `sqrt_scale1000_x50` | 0.998 [0.994,1.001] | 1.000 [0.993,1.009] | 1.003 [0.996,1.016] | 1.000 [0.983,1.003] |
| `sqrt_scale5000_x5` | 1.006 [0.998,1.007] | 0.999 [0.995,1.001] | 1.002 [1.001,1.007] | 1.000 [0.995,1.004] |
| `multiply_dense_1000d_x5000` | 0.996 [0.990,1.008] | 0.999 [0.993,1.008] | 1.004 [0.995,1.008] | 0.998 [0.989,1.010] |
| `multiply_dense_10000d_x500` | 0.881 [0.872,0.886] | 0.997 [0.992,0.999] | 1.002 [0.998,1.014] | 0.997 [0.997,1.003] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.664 [0.662,0.676] | 1.004 [0.994,1.011] | 1.002 [0.990,1.018] | 0.999 [0.992,1.008] |
| `multiply_sparse_10000d_x1000` | 0.777 [0.768,0.781] | 1.001 [1.000,1.014] | 0.998 [0.997,1.005] | 1.002 [0.998,1.009] |
| `divide_multilimb_900d_450d_s100_x1000` | 0.958 [0.927,1.002] | 1.015 [0.968,1.059] | 0.978 [0.976,1.055] | 1.014 [0.956,1.063] |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.014 [0.988,1.039] | 0.992 [0.965,1.014] | 0.999 [0.990,1.023] | 0.989 [0.956,1.016] |
| `modulo_1000d_350d_x3000` | 0.994 [0.974,0.999] | 1.005 [0.999,1.012] | 0.992 [0.985,1.000] | 1.004 [0.997,1.031] |
| `power_100d_e17_x1000` | 1.002 [0.990,1.007] | 1.000 [0.996,1.003] | 0.998 [0.994,1.013] | 0.997 [0.986,1.022] |
| `power_20d_e32_x2000` | 1.000 [0.991,1.017] | 1.006 [0.994,1.021] | 0.994 [0.970,1.015] | 1.002 [0.988,1.022] |
| `ibase_literal_flip_20000` | 1.005 [0.985,1.011] | 0.844 [0.835,0.853] | 0.955 [0.944,0.969] | 1.009 [0.989,1.031] |
| `recursion_50_x1000` | 0.997 [0.991,1.006] | 0.782 [0.774,0.785] | 0.959 [0.954,0.964] | 0.998 [0.991,1.023] |
| `sparse_array_200000` | 0.998 [0.993,1.001] | 1.032 [1.025,1.035] | 0.922 [0.918,0.930] | 1.008 [1.004,1.012] |
| `frontend_definitions_2000` | 0.994 [0.977,0.996] | 0.996 [0.979,1.034] | 1.003 [0.957,1.009] | 1.000 [0.996,1.005] |
| `decimal_input_4000d_x100` | 0.143 [0.143,0.146] | 1.001 [0.988,1.004] | 0.999 [0.994,1.007] | 1.009 [0.979,1.024] |
| `decimal_output_200d_x1000` | 0.996 [0.993,1.000] | 1.007 [1.001,1.008] | 1.008 [0.996,1.069] | 0.985 [0.984,1.005] |
| `streaming_statements_20000` | 0.989 [0.976,0.999] | 0.994 [0.986,1.030] | 1.009 [0.971,1.021] | 0.990 [0.984,0.999] |
| `square_100d_x100000` | 0.997 [0.995,1.001] | 1.022 [1.019,1.027] | 0.968 [0.963,0.970] | 0.998 [0.998,1.003] |
| `square_1000d_x10000` | 0.995 [0.992,1.000] | 1.001 [0.999,1.004] | 0.997 [0.995,1.004] | 1.001 [0.996,1.004] |
| `square_10000d_x500` | 0.966 [0.959,0.973] | 0.998 [0.993,1.003] | 0.997 [0.991,1.032] | 1.001 [0.965,1.005] |
| `literal_repeat_2000d_x2000` | 0.893 [0.829,0.924] | 1.017 [0.980,1.045] | 0.973 [0.954,1.032] | 1.003 [0.987,1.014] |

## Holdout adjacent ratios

| Workload | Decimal / accepted | Frames / decimal | Verified / frames | Arrays / verified |
| --- | --- | --- | --- | --- |
| `startup` | 1.034 [0.975,1.058] | 1.024 [0.985,1.076] | 0.968 [0.911,1.000] | 0.989 [0.985,1.050] |
| `loop_875000` | 0.991 [0.987,0.997] | 1.038 [1.032,1.046] | 0.921 [0.914,0.928] | 1.001 [0.999,1.012] |
| `calls_175000` | 0.998 [0.994,1.008] | 0.797 [0.791,0.799] | 0.922 [0.921,0.924] | 1.001 [0.998,1.009] |
| `array_175000` | 1.005 [0.993,1.011] | 1.012 [0.997,1.025] | 0.959 [0.945,0.967] | 0.817 [0.802,0.823] |
| `compare_high_1125d_x87500` | 0.983 [0.973,0.991] | 1.034 [1.032,1.059] | 0.952 [0.936,0.960] | 1.003 [0.971,1.031] |
| `compare_low_1125d_x87500` | 0.986 [0.975,0.992] | 1.026 [1.013,1.042] | 0.970 [0.947,0.974] | 1.000 [0.991,1.020] |
| `compare_equal_1125d_x87500` | 1.003 [0.981,1.012] | 1.025 [1.006,1.061] | 0.970 [0.948,0.987] | 0.990 [0.972,1.000] |
| `fibonacci_1125_x88` | 0.999 [0.995,1.002] | 1.036 [1.034,1.050] | 0.926 [0.919,0.932] | 1.007 [0.998,1.014] |
| `fibonacci_11250_x9` | 0.999 [0.986,1.004] | 1.020 [1.017,1.025] | 0.956 [0.920,0.962] | 1.000 [0.999,1.003] |
| `factorial_1125_x44` | 0.997 [0.995,1.000] | 1.012 [1.010,1.024] | 0.975 [0.963,0.976] | 0.999 [0.995,1.006] |
| `factorial_5625_x5` | 0.999 [0.991,1.003] | 1.003 [0.998,1.006] | 0.994 [0.992,0.994] | 1.002 [0.997,1.003] |
| `multiply_112d_x87500` | 0.997 [0.989,0.999] | 1.022 [1.015,1.049] | 0.957 [0.946,0.962] | 1.006 [0.999,1.025] |
| `multiply_1125d_x8750` | 0.998 [0.984,1.003] | 0.998 [0.992,1.002] | 1.000 [0.998,1.003] | 1.000 [0.995,1.006] |
| `multiply_11250d_x875` | 0.921 [0.912,0.924] | 0.999 [0.997,1.011] | 1.001 [0.992,1.006] | 1.000 [0.997,1.007] |
| `divide_scale112_x87500` | 0.995 [0.983,0.998] | 1.017 [1.015,1.030] | 0.944 [0.941,0.948] | 1.002 [0.995,1.007] |
| `divide_scale1125_x8750` | 0.997 [0.953,1.006] | 1.008 [0.994,1.025] | 0.988 [0.984,1.025] | 1.003 [0.983,1.015] |
| `divide_scale5625_x4375` | 1.001 [0.997,1.016] | 1.001 [0.998,1.009] | 0.988 [0.982,0.995] | 1.000 [0.995,1.020] |
| `sqrt_scale112_x4375` | 0.998 [0.985,1.005] | 1.003 [0.998,1.050] | 0.994 [0.939,0.999] | 1.004 [0.999,1.055] |
| `sqrt_scale1125_x44` | 1.000 [0.994,1.003] | 0.998 [0.993,1.000] | 1.002 [0.997,1.011] | 0.998 [0.988,1.004] |
| `sqrt_scale5625_x5` | 0.998 [0.998,1.020] | 0.999 [0.984,1.001] | 1.002 [0.998,1.003] | 1.000 [0.998,1.007] |
| `multiply_dense_1125d_x4375` | 0.995 [0.988,1.000] | 0.999 [0.989,1.008] | 1.004 [0.989,1.006] | 1.007 [0.999,1.008] |
| `multiply_dense_11250d_x438` | 0.852 [0.850,0.865] | 1.000 [0.977,1.000] | 1.001 [0.997,1.009] | 1.000 [0.989,1.006] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.623 [0.578,0.636] | 0.996 [0.980,1.002] | 1.005 [0.992,1.024] | 0.996 [0.985,1.002] |
| `multiply_sparse_11250d_x875` | 0.740 [0.739,0.765] | 0.998 [0.968,1.001] | 1.001 [0.999,1.009] | 1.001 [0.994,1.005] |
| `divide_multilimb_1012d_506d_s112_x875` | 0.933 [0.870,0.950] | 1.026 [0.937,1.034] | 1.041 [0.970,1.064] | 1.041 [0.944,1.110] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.042 [1.016,1.055] | 1.007 [0.967,1.028] | 0.990 [0.976,1.027] | 0.989 [0.982,0.998] |
| `modulo_1125d_393d_x2625` | 1.070 [0.991,1.093] | 1.005 [0.988,1.113] | 1.003 [0.882,1.060] | 1.019 [0.956,1.059] |
| `power_112d_e17_x875` | 1.004 [0.998,1.008] | 0.998 [0.995,1.003] | 1.000 [0.995,1.009] | 1.002 [0.993,1.003] |
| `power_22d_e32_x1750` | 1.005 [0.998,1.014] | 1.006 [0.977,1.025] | 0.985 [0.962,1.006] | 1.002 [0.987,1.053] |
| `ibase_literal_flip_17500` | 1.001 [0.964,1.005] | 0.847 [0.828,0.852] | 0.965 [0.961,0.968] | 1.000 [0.973,1.027] |
| `recursion_56_x875` | 0.998 [0.989,1.002] | 0.784 [0.781,0.786] | 0.956 [0.953,0.959] | 1.004 [1.000,1.012] |
| `sparse_array_175000` | 0.995 [0.991,0.997] | 1.031 [1.028,1.041] | 0.925 [0.916,0.930] | 1.011 [1.003,1.017] |
| `frontend_definitions_1750` | 1.002 [0.985,1.015] | 0.997 [0.988,1.010] | 0.999 [0.997,1.007] | 0.999 [0.986,1.004] |
| `decimal_input_4500d_x88` | 0.128 [0.128,0.128] | 1.005 [1.000,1.023] | 1.002 [0.990,1.012] | 1.022 [0.993,1.036] |
| `decimal_output_225d_x875` | 0.999 [0.991,1.005] | 1.005 [0.993,1.014] | 0.998 [0.994,1.053] | 1.003 [0.969,1.017] |
| `streaming_statements_17500` | 0.989 [0.974,0.996] | 1.001 [0.992,1.004] | 1.005 [0.999,1.018] | 0.992 [0.988,0.999] |
| `square_112d_x87500` | 0.997 [0.992,1.001] | 1.017 [1.009,1.024] | 0.974 [0.968,0.985] | 1.000 [0.992,1.005] |
| `square_1125d_x8750` | 0.998 [0.992,1.001] | 0.997 [0.990,1.002] | 0.998 [0.992,1.002] | 1.009 [1.003,1.012] |
| `square_11250d_x438` | 0.958 [0.950,0.960] | 0.998 [0.993,1.001] | 1.001 [0.996,1.005] | 1.002 [0.995,1.002] |
| `literal_repeat_2250d_x1750` | 0.856 [0.827,0.891] | 1.007 [0.993,1.057] | 0.993 [0.939,1.024] | 0.992 [0.979,1.029] |

Math-library classes, alternative backends, final compiler/PGO and complete
cross-platform calibration remain pending in their own issues. Their absence
is unsupported/pending, never a performance pass.
