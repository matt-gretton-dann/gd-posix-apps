# Exclusive named-file input: accepted bounded stage

Refs #210, #216 and #197. PR #352 accepts candidate
`18da3049afdd10747268d95ba0ebf10a7e84fdb2` against actual local/native base
`d252da1f8d3e4e5d37dede1724851c9b004c8147`. Actual source identities, fixed
split 126, extensions ON, compiler/library/flags, binary hashes and commands
remain in the immutable recipes. Local host: Apple M1/macOS 27.2, Apple Clang
21/libc++ 220106, Release thin LTO. No instrumentation is used for speed.

Named streams privately owned by FileReader opt into an exclusive stdio read.
Borrowed stdin and existing default callers retain locked getc. Supported POSIX
and MSVC use their documented unlocked primitives; other platforms use fgetc.
EOF, errors, retry behavior and text translation share the same loop. There is
no new read-ahead, scanner, batching, output delay or signal-lifetime policy.

## Correctness and serial timing

Release passes 214 numeric/VM cases/692004 assertions, 9 utility cases/8232
assertions and eight CLI/helper groups. The binary-byte tests cover all 256
unsigned values, alternating APIs, EOF and line interoperability. Extensions-OFF
ASan/UBSan utility, CLI, VM CLI and quick benchmark transport tests pass. The
final prerequisite integration repeats all numeric/VM and eight helper groups.

Full and held-out each check 1656 invocations (46 cases, two roles, three
warmups and fifteen shuffled paired samples), 3312 exact output/status/stderr/
deadline checks total. Schedule seeds 197210/197211 differ from corpus seeds
197200/197201. All intervals and losing medians are retained below. No class
has CI95 wholly above 1.05. No builds, tests, profiles or RSS collection overlap
these timings. Host scheduling, frequency and temperature remain uncontrolled.

File input is 0.965 [0.951,0.966] full and 0.974 [0.959,0.999] held-out.
Stdin streaming remains about 1.002 in both. This is a modest local file-input
gain, not a universal improvement. The file artifact is validated before the
timer, so this measures cached file/frontend/VM work rather than cold disk I/O.

## Separate memory and native checks

All 276 serial fresh-process RSS executions pass exact checks, using three
processes per full case and role. File-case median peaks are both 2162688 bytes;
stdin streaming medians are 2228224/2211840. Peaks include loader, frontend,
VM, values, output and allocator; they do not isolate reader storage. Three
samples do not establish portable memory equivalence. Local getrusage(2) byte
units are retained. The first sandboxed resource-collection attempt exited
before collecting a valid sample; the documented collector then ran with
process-resource access. Neither attempt contributes release speed samples.

All fourteen source checks pass. Three digest/source-verified native archives
contain 1750 checked invocations each (5250 total), with no confirmed regression.
File ratios: Linux 0.996 [0.983,1.007], macOS 0.986 [0.954,1.198], Windows
1.003 [0.978,1.020]. Native results are neutral or noisy, not evidence of a
portable gain. Linux loop 1.037 [1.017,1.063] and macOS loop 1.054
[1.016,1.168] remain inconclusive and visible in the full summaries.

Adopt this small opt-in stage on the matched local evidence and correctness
checks, keeping the generic fallback. Scanner/refill, bounded batching, driver
signal policy and interactive behavior remain #210 work. Overall competitive
acceptance, coverage recalculation and #197 completion remain open.

## full paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.018 [0.902,1.077] |
| `loop_1000000` | 1.000 [0.990,1.005] |
| `calls_200000` | 1.000 [0.985,1.007] |
| `array_200000` | 1.009 [1.004,1.017] |
| `compare_high_1000d_x100000` | 0.994 [0.986,1.009] |
| `compare_low_1000d_x100000` | 1.001 [0.994,1.020] |
| `compare_equal_1000d_x100000` | 0.997 [0.990,1.010] |
| `fibonacci_1000_x100` | 1.001 [0.999,1.017] |
| `fibonacci_10000_x10` | 0.999 [0.996,1.011] |
| `factorial_1000_x50` | 1.003 [0.999,1.024] |
| `factorial_5000_x5` | 0.997 [0.993,1.003] |
| `multiply_100d_x100000` | 0.995 [0.988,1.001] |
| `multiply_1000d_x10000` | 0.998 [0.996,1.005] |
| `multiply_10000d_x1000` | 1.003 [1.000,1.010] |
| `divide_scale100_x100000` | 0.994 [0.984,1.008] |
| `divide_scale1000_x10000` | 1.004 [0.989,1.017] |
| `divide_scale5000_x5000` | 0.998 [0.990,1.004] |
| `sqrt_scale100_x5000` | 0.997 [0.994,1.011] |
| `sqrt_scale1000_x50` | 1.005 [1.000,1.011] |
| `sqrt_scale5000_x5` | 1.000 [0.997,1.006] |
| `multiply_dense_1000d_x5000` | 0.999 [0.992,1.003] |
| `multiply_dense_10000d_x500` | 0.996 [0.994,1.001] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.013 [1.008,1.015] |
| `multiply_sparse_10000d_x1000` | 0.998 [0.994,1.005] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.024 [0.992,1.038] |
| `divide_multilimb_1000d_900d_s1000_x500` | 1.021 [1.011,1.037] |
| `modulo_1000d_350d_x3000` | 1.007 [0.977,1.016] |
| `power_100d_e17_x1000` | 0.994 [0.985,1.011] |
| `power_20d_e32_x2000` | 0.993 [0.918,1.017] |
| `ibase_literal_flip_20000` | 1.001 [0.991,1.012] |
| `recursion_50_x1000` | 0.995 [0.991,0.997] |
| `sparse_array_200000` | 0.998 [0.981,1.013] |
| `frontend_definitions_2000` | 1.001 [0.995,1.010] |
| `decimal_input_4000d_x100` | 1.005 [0.999,1.025] |
| `decimal_output_200d_x1000` | 0.999 [0.966,1.005] |
| `streaming_statements_20000` | 1.002 [0.998,1.013] |
| `square_100d_x100000` | 1.001 [0.997,1.003] |
| `square_1000d_x10000` | 1.000 [0.999,1.002] |
| `square_10000d_x500` | 0.999 [0.990,1.003] |
| `literal_repeat_2000d_x2000` | 0.990 [0.935,1.140] |
| `binary_output_1000d_x300` | 0.989 [0.981,1.002] |
| `octal_output_1000d_x300` | 1.003 [0.988,1.015] |
| `binary_input_100000d_x3` | 1.011 [1.000,1.015] |
| `hex_input_100000d_x3` | 1.005 [1.003,1.007] |
| `temporary_chain_1000d_x50000` | 1.001 [0.997,1.003] |
| `file_statements_20000` | 0.965 [0.951,0.966] |

## holdout paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 0.986 [0.955,1.107] |
| `loop_875000` | 1.001 [0.995,1.017] |
| `calls_175000` | 1.001 [0.994,1.011] |
| `array_175000` | 1.000 [0.997,1.006] |
| `compare_high_1125d_x87500` | 0.990 [0.975,1.010] |
| `compare_low_1125d_x87500` | 0.997 [0.984,1.007] |
| `compare_equal_1125d_x87500` | 0.993 [0.976,1.006] |
| `fibonacci_1125_x88` | 0.999 [0.996,1.008] |
| `fibonacci_11250_x9` | 1.000 [0.994,1.005] |
| `factorial_1125_x44` | 0.997 [0.977,0.998] |
| `factorial_5625_x5` | 1.002 [1.001,1.004] |
| `multiply_112d_x87500` | 1.004 [0.993,1.007] |
| `multiply_1125d_x8750` | 0.992 [0.990,0.996] |
| `multiply_11250d_x875` | 0.999 [0.993,1.010] |
| `divide_scale112_x87500` | 0.997 [0.982,1.009] |
| `divide_scale1125_x8750` | 1.012 [0.969,1.035] |
| `divide_scale5625_x4375` | 0.994 [0.967,0.999] |
| `sqrt_scale112_x4375` | 0.997 [0.996,1.000] |
| `sqrt_scale1125_x44` | 1.006 [0.999,1.009] |
| `sqrt_scale5625_x5` | 1.002 [0.998,1.003] |
| `multiply_dense_1125d_x4375` | 0.990 [0.978,0.992] |
| `multiply_dense_11250d_x438` | 0.999 [0.991,1.004] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.003 [0.998,1.007] |
| `multiply_sparse_11250d_x875` | 1.001 [0.998,1.003] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.010 [0.985,1.074] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.006 [0.991,1.034] |
| `modulo_1125d_393d_x2625` | 1.014 [1.000,1.062] |
| `power_112d_e17_x875` | 1.002 [0.992,1.007] |
| `power_22d_e32_x1750` | 0.991 [0.972,0.999] |
| `ibase_literal_flip_17500` | 1.004 [0.975,1.029] |
| `recursion_56_x875` | 0.993 [0.976,0.996] |
| `sparse_array_175000` | 1.000 [0.990,1.012] |
| `frontend_definitions_1750` | 1.003 [0.995,1.031] |
| `decimal_input_4500d_x88` | 1.003 [0.991,1.008] |
| `decimal_output_225d_x875` | 0.992 [0.965,1.021] |
| `streaming_statements_17500` | 1.002 [0.991,1.008] |
| `square_112d_x87500` | 1.002 [0.999,1.009] |
| `square_1125d_x8750` | 1.007 [1.002,1.009] |
| `square_11250d_x438` | 0.998 [0.997,1.003] |
| `literal_repeat_2250d_x1750` | 1.013 [0.976,1.071] |
| `binary_output_1125d_x263` | 1.020 [1.008,1.024] |
| `octal_output_1125d_x263` | 0.993 [0.985,1.005] |
| `binary_input_112500d_x3` | 1.004 [0.993,1.007] |
| `hex_input_112500d_x3` | 1.001 [0.998,1.005] |
| `temporary_chain_1125d_x43750` | 0.998 [0.997,1.003] |
| `file_statements_17500` | 0.974 [0.959,0.999] |
