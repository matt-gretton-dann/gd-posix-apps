# Bounded parser instruction reuse — accepted #210 stage

Refs #197. PR #346 accepts candidate `9c65f912ac9ff5c32503f8d25d404e439164297f`.
Local baseline is `7974cb3d8b16c98441f9dc960f6c91f52736b06d`; native baseline is
`ac5eee3cbc494ac3c5f63ce2bd2e9b71cc74271f`. Their non-document trees match,
but actual revision labels remain separate.

Only a sole shared owner is reused. Clearing destroys all old instructions and
literal caches; retained caller snapshots still get independent streams. Function
registration copies persistent bodies, and VM teardown clears borrowed results
before the next parse. Item boundaries, read-time quit, observable output, error
timing and signal lifecycle are unchanged. This stage adds no scanner or batching.

## Correctness and scoped counters

The test-first allocation regression records 194 requests for 64 one-line items
and 3074 for 1024. After reuse, each needs three allocations/384 selected bytes,
with exact repeated output and balanced storage. This is selected instruction
storage, not process malloc traffic or RSS. Retained-result fixtures keep several
old streams alive across more parses and validate their contents/identities.

Release passes 213 cases/691956 assertions and eight CLI/helper groups. The
profiling build passes 222 cases/692010 assertions and fifteen CLI groups.
Extensions-OFF ASan/UBSan passes 38 parser/VM cases/56643 assertions, 26 CLI and
fifteen VM CLI tests, including recursive frames and read-time quit. All 45 quick
release workload oracles pass. Profile timings are not performance evidence.

## Matched local timing

Apple M1/macOS 27.2, Apple Clang 21/libc++ 220106, Release thin LTO, split 126,
extensions ON. Full and held-out each check 900 process invocations: 45 cases,
two roles, one warmup and nine shuffled paired samples. All 1800 pass exact
output/status/stderr/deadline checks. Schedule seeds 197233/197234 and corpus
seeds 197200/197201 are separate; CI95 uses 2000 bootstrap resamples. No builds,
tests, profiles or memory diagnostics overlap timings. Host scheduling/frequency/
thermals remain uncontrolled. Every class and losing/noisy interval is retained
below. Process timings include startup and output; controls are not subtracted.

Streaming ratios are 0.938 [0.929,0.949] full and 0.942 [0.929,0.955] held-out.
Repeated definitions are 0.970 [0.966,0.982] and 0.961 [0.932,0.978]. No class
has CI95 wholly above 1.05. Held-out loop retains 1.010 [1.007,1.013]; calls
1.010 [1.000,1.014]. This is a bounded frontend gain, not universal neutrality.

## Separate peak and retained memory

All 270 fresh-process memory runs (three per full case/role) pass exact checks.
Raw getrusage maximum RSS is bytes under the retained local reference. Streaming
sampled medians are 2359296/2293760 bytes and definitions 2441216/2375680.
Calls stay 2015232/2015232; dense arrays are 24281088/24264704. Three samples
do not establish portable peaks or confidence intervals, and smaller counters
are not automatically smaller process memory.

Capacity retains the largest parsed item, not all items. Six separate live-RSS
diagnostics exercise a 65536-assignment item followed by 1024 small items with
exact 123/456 output checkpoints. stdout uses a PTY for line buffering; sampling
occurs while stdin stays open and bc waits for further input. Local ps RSS is
in KiB and is explicitly converted to bytes; the source reference is retained.
Sampled medians after the large item are 94814208 bytes in both implementations;
after small items 95076352/95043584. Both include the existing VM result pool
and allocator behavior: they do not isolate retained parser capacity or prove
its absence. The scripts, programs, samples and limitations remain reviewable.

## Native acceptance and limits

All fourteen CI/native/CodeQL checks pass at the exact candidate. Digest-verified
native ZIPs contain 1700 checked rows each (5100 total), no confirmed regression.
Linux definitions/streaming ratios are 0.905/0.942 and Windows 0.939/0.884.
Linux loop retains 1.025 [1.014,1.038]; macOS intervals are wide/noisy (including
definitions 1.028 [0.801,1.363]), and Windows array is also inconclusive. Do not
convert advisory/noisy outcomes into universal speed or memory claims.

Adopt this scoped stage. Scanner/refill, bounded batch/signal policy and I/O
experiments remain #210 work. File-input workloads must be distinguished from
the stdin corpus before making file-specific claims. #197 final coverage and
the overall competitive matrix remain open.

## full paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 0.987 [0.950,1.097] |
| `loop_1000000` | 1.005 [0.999,1.008] |
| `calls_200000` | 1.012 [0.993,1.021] |
| `array_200000` | 0.996 [0.989,1.010] |
| `compare_high_1000d_x100000` | 1.009 [1.000,1.022] |
| `compare_low_1000d_x100000` | 0.993 [0.980,0.999] |
| `compare_equal_1000d_x100000` | 0.990 [0.970,1.020] |
| `fibonacci_1000_x100` | 1.000 [0.997,1.006] |
| `fibonacci_10000_x10` | 0.999 [0.990,1.013] |
| `factorial_1000_x50` | 0.997 [0.992,1.010] |
| `factorial_5000_x5` | 1.000 [0.995,1.003] |
| `multiply_100d_x100000` | 1.005 [0.999,1.014] |
| `multiply_1000d_x10000` | 0.998 [0.996,1.004] |
| `multiply_10000d_x1000` | 1.007 [1.000,1.009] |
| `divide_scale100_x100000` | 0.990 [0.980,0.995] |
| `divide_scale1000_x10000` | 0.990 [0.947,1.004] |
| `divide_scale5000_x5000` | 0.999 [0.982,1.021] |
| `sqrt_scale100_x5000` | 1.001 [0.995,1.005] |
| `sqrt_scale1000_x50` | 0.999 [0.984,1.005] |
| `sqrt_scale5000_x5` | 1.001 [0.995,1.004] |
| `multiply_dense_1000d_x5000` | 0.992 [0.977,0.994] |
| `multiply_dense_10000d_x500` | 0.999 [0.997,1.002] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.001 [0.988,1.008] |
| `multiply_sparse_10000d_x1000` | 1.002 [0.996,1.004] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.006 [0.966,1.040] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.991 [0.983,1.010] |
| `modulo_1000d_350d_x3000` | 1.000 [0.993,1.002] |
| `power_100d_e17_x1000` | 1.007 [0.993,1.021] |
| `power_20d_e32_x2000` | 0.997 [0.986,1.025] |
| `ibase_literal_flip_20000` | 0.995 [0.978,1.033] |
| `recursion_50_x1000` | 1.014 [1.008,1.019] |
| `sparse_array_200000` | 1.005 [0.994,1.021] |
| `frontend_definitions_2000` | 0.970 [0.966,0.982] |
| `decimal_input_4000d_x100` | 1.000 [0.995,1.027] |
| `decimal_output_200d_x1000` | 0.994 [0.912,1.024] |
| `streaming_statements_20000` | 0.938 [0.929,0.949] |
| `square_100d_x100000` | 1.002 [1.001,1.004] |
| `square_1000d_x10000` | 1.001 [0.997,1.004] |
| `square_10000d_x500` | 1.000 [0.996,1.005] |
| `literal_repeat_2000d_x2000` | 0.989 [0.953,1.059] |
| `binary_output_1000d_x300` | 1.021 [0.986,1.113] |
| `octal_output_1000d_x300` | 1.019 [0.952,1.071] |
| `binary_input_100000d_x3` | 0.996 [0.980,1.014] |
| `hex_input_100000d_x3` | 1.000 [0.996,1.007] |
| `temporary_chain_1000d_x50000` | 0.999 [0.998,1.004] |

## holdout paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 0.998 [0.989,1.096] |
| `loop_875000` | 1.010 [1.007,1.013] |
| `calls_175000` | 1.010 [1.000,1.014] |
| `array_175000` | 1.000 [0.991,1.020] |
| `compare_high_1125d_x87500` | 1.006 [0.995,1.024] |
| `compare_low_1125d_x87500` | 1.000 [0.974,1.014] |
| `compare_equal_1125d_x87500` | 1.008 [0.983,1.011] |
| `fibonacci_1125_x88` | 1.000 [0.985,1.001] |
| `fibonacci_11250_x9` | 0.997 [0.993,1.002] |
| `factorial_1125_x44` | 1.000 [0.997,1.006] |
| `factorial_5625_x5` | 1.001 [0.996,1.004] |
| `multiply_112d_x87500` | 1.004 [1.000,1.011] |
| `multiply_1125d_x8750` | 1.000 [0.992,1.004] |
| `multiply_11250d_x875` | 1.008 [1.005,1.011] |
| `divide_scale112_x87500` | 0.993 [0.986,0.996] |
| `divide_scale1125_x8750` | 1.010 [0.991,1.049] |
| `divide_scale5625_x4375` | 0.998 [0.992,1.026] |
| `sqrt_scale112_x4375` | 0.999 [0.994,1.015] |
| `sqrt_scale1125_x44` | 1.001 [0.992,1.008] |
| `sqrt_scale5625_x5` | 1.001 [0.995,1.004] |
| `multiply_dense_1125d_x4375` | 1.002 [0.995,1.014] |
| `multiply_dense_11250d_x438` | 1.003 [1.000,1.011] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.010 [0.998,1.022] |
| `multiply_sparse_11250d_x875` | 1.001 [0.998,1.009] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.046 [0.994,1.062] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.000 [0.985,1.012] |
| `modulo_1125d_393d_x2625` | 0.996 [0.982,1.059] |
| `power_112d_e17_x875` | 0.995 [0.983,1.025] |
| `power_22d_e32_x1750` | 0.990 [0.974,1.008] |
| `ibase_literal_flip_17500` | 0.987 [0.968,1.027] |
| `recursion_56_x875` | 0.999 [0.995,1.011] |
| `sparse_array_175000` | 0.998 [0.988,1.005] |
| `frontend_definitions_1750` | 0.961 [0.932,0.978] |
| `decimal_input_4500d_x88` | 0.999 [0.987,1.010] |
| `decimal_output_225d_x875` | 1.003 [0.978,1.012] |
| `streaming_statements_17500` | 0.942 [0.929,0.955] |
| `square_112d_x87500` | 1.001 [1.000,1.016] |
| `square_1125d_x8750` | 1.003 [0.997,1.004] |
| `square_11250d_x438` | 1.001 [0.997,1.002] |
| `literal_repeat_2250d_x1750` | 1.016 [0.997,1.031] |
| `binary_output_1125d_x263` | 1.023 [0.978,1.043] |
| `octal_output_1125d_x263` | 0.993 [0.979,0.999] |
| `binary_input_112500d_x3` | 0.998 [0.995,1.004] |
| `hex_input_112500d_x3` | 0.999 [0.996,1.003] |
| `temporary_chain_1125d_x43750` | 1.001 [0.992,1.002] |
