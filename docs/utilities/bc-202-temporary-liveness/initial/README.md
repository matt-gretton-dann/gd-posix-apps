# Initial arithmetic temporary-consumption candidate

Refs #202, #207 and #197. Exact baseline
`e53a4b9fb73fc037bc4e30aecb310fbff37b92a2`; exact candidate
`34a10afa4fafe46bbd7668a0738e4d8a4c5ce913`. Both use existing verified dispatch.
All actual source/compiler/library/flag identities, commands and binary hashes
remain in release and separate profile recipes. No later policy is substituted.

Apple M1/macOS 27.2, Apple Clang 21, Release thin LTO, split 126, extensions ON.
Full 900 and held-out 900 process invocations pass exact independent oracles
and status/stderr/deadline checks. One checked warmup and nine shuffled paired
rounds, separate corpus/schedule seeds and 2000 bootstrap resamples. Profiling
is OFF for speed measurements. No local builds/tests/profiles/memory diagnostics
overlap timings; host scheduling/frequency/thermals remain uncontrolled. All 45
cases and every losing or inconclusive interval remain below, without averaging
away classes. Controls are not subtracted from end-to-end process measurements.

The temporary chain is 0.868 [0.865,0.873] of baseline; held-out is 0.875
[0.865,0.877]. The million-iteration scalar loop is 1.006 [1.003,1.008], calls
about 0.999, and definitions 1.037 [1.024,1.064] (held-out 1.028 [1.009,1.044]).
No local interval is wholly above 1.05, but this is not universal neutrality.

## Separate counters, memory and native evidence

Ninety instrumented executions (all full cases on both sources) pass exact
oracles and balanced selected-storage checks. Never use their elapsed time as
speed evidence. The chain drops from 350000 to 50000 COW clones and from
156800000 to 22400000 copied limb bytes; total selected allocated bytes drop
from 173627656 to 24827741. VM instruction counts are unchanged. Scalar loops
have zero clones/owned-digit copies in both sources, yet the candidate moves
3000001 inline temporaries. Definitions execute zero transfers but request
60000 extra tracked plan bytes. Plan-construction temporaries and owner headers
are outside selected byte counters, not outside process memory.

Two hundred seventy separate fresh-process RSS runs (three per case/source)
pass exact output/status/stderr checks after all speed/counter work. They use
the local byte-valued getrusage convention and preserve every resource log.
Process peaks include loader, parsing, code, VM state, values and output; they
are not selected allocator payload or isolated workspace. Sampled medians are
unchanged for the chain (1.969 MiB), loop (1.875) and dense array (23.141);
calls are 1.906/1.922 and definitions 2.281/2.344 MiB. Three samples establish
neither portable peaks nor a confidence interval. Both __TEXT segments are
311296 bytes; executable file bytes are 507776/508048. __TEXT is not RSS.

All fourteen native/CI/CodeQL checks at this head pass. Digest-verified native
ZIPs have 1730 Linux and 1700 macOS/Windows checked rows, with no confirmed
regressions under the policy. Linux definitions initially measure 1.057
[1.054,1.067], but bounded confirmation leaves the outcome inconclusive.
Windows calls and several macOS intervals also remain noisy/inconclusive.
The chain passes at 0.910/0.892/0.842 across Linux/macOS/Windows; retain the
full intervals and advisory classifications rather than claiming all are faster.

## Decision: retain as an initial candidate, improve avoidable work

The correctness/block proof and clone saving are supported. Before adopting
the stage, evaluate skipping inline transfers (no COW benefit) and avoiding
plans for outer function bodies that are registered rather than executed.
The observed overhead is retained; no compiler/hardware cause is asserted.
Any changed source needs new matched/native evaluation. #202 and #207 stay open.

## full paired intervals

| Workload | Candidate / baseline, 95% interval |
| --- | --- |
| `startup` | 0.921 [0.873,1.160] |
| `loop_1000000` | 1.006 [1.003,1.008] |
| `calls_200000` | 0.999 [0.983,1.013] |
| `array_200000` | 1.003 [0.992,1.014] |
| `compare_high_1000d_x100000` | 0.994 [0.989,1.015] |
| `compare_low_1000d_x100000` | 1.009 [0.987,1.023] |
| `compare_equal_1000d_x100000` | 0.995 [0.982,1.014] |
| `fibonacci_1000_x100` | 0.981 [0.969,0.992] |
| `fibonacci_10000_x10` | 1.009 [1.000,1.019] |
| `factorial_1000_x50` | 0.990 [0.984,1.003] |
| `factorial_5000_x5` | 0.999 [0.988,1.017] |
| `multiply_100d_x100000` | 1.015 [1.003,1.018] |
| `multiply_1000d_x10000` | 0.999 [0.991,1.009] |
| `multiply_10000d_x1000` | 0.998 [0.987,1.003] |
| `divide_scale100_x100000` | 1.003 [0.979,1.035] |
| `divide_scale1000_x10000` | 1.000 [0.951,1.022] |
| `divide_scale5000_x5000` | 1.007 [0.978,1.031] |
| `sqrt_scale100_x5000` | 1.004 [0.994,1.008] |
| `sqrt_scale1000_x50` | 0.999 [0.984,1.001] |
| `sqrt_scale5000_x5` | 1.009 [1.003,1.021] |
| `multiply_dense_1000d_x5000` | 1.007 [0.991,1.022] |
| `multiply_dense_10000d_x500` | 0.995 [0.993,1.000] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.998 [0.984,1.016] |
| `multiply_sparse_10000d_x1000` | 0.997 [0.990,1.008] |
| `divide_multilimb_900d_450d_s100_x1000` | 0.965 [0.952,1.008] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.994 [0.977,1.043] |
| `modulo_1000d_350d_x3000` | 0.990 [0.978,0.996] |
| `power_100d_e17_x1000` | 1.013 [0.984,1.028] |
| `power_20d_e32_x2000` | 1.031 [0.974,1.047] |
| `ibase_literal_flip_20000` | 1.026 [0.979,1.058] |
| `recursion_50_x1000` | 0.983 [0.956,1.007] |
| `sparse_array_200000` | 1.005 [0.987,1.016] |
| `frontend_definitions_2000` | 1.037 [1.024,1.064] |
| `decimal_input_4000d_x100` | 0.999 [0.994,1.009] |
| `decimal_output_200d_x1000` | 0.936 [0.880,1.076] |
| `streaming_statements_20000` | 1.003 [0.999,1.010] |
| `square_100d_x100000` | 1.013 [0.997,1.014] |
| `square_1000d_x10000` | 0.997 [0.990,1.001] |
| `square_10000d_x500` | 1.004 [0.998,1.006] |
| `literal_repeat_2000d_x2000` | 0.942 [0.885,1.153] |
| `binary_output_1000d_x300` | 1.020 [0.983,1.042] |
| `octal_output_1000d_x300` | 0.999 [0.968,1.030] |
| `binary_input_100000d_x3` | 1.001 [0.987,1.006] |
| `hex_input_100000d_x3` | 0.997 [0.993,1.006] |
| `temporary_chain_1000d_x50000` | 0.868 [0.865,0.873] |

## holdout paired intervals

| Workload | Candidate / baseline, 95% interval |
| --- | --- |
| `startup` | 1.088 [1.002,1.217] |
| `loop_875000` | 1.004 [1.001,1.011] |
| `calls_175000` | 1.001 [0.997,1.008] |
| `array_175000` | 0.998 [0.983,1.018] |
| `compare_high_1125d_x87500` | 0.985 [0.969,1.014] |
| `compare_low_1125d_x87500` | 1.004 [0.996,1.011] |
| `compare_equal_1125d_x87500` | 1.009 [0.996,1.021] |
| `fibonacci_1125_x88` | 0.980 [0.960,1.000] |
| `fibonacci_11250_x9` | 1.000 [0.981,1.013] |
| `factorial_1125_x44` | 0.984 [0.983,0.989] |
| `factorial_5625_x5` | 0.995 [0.994,1.001] |
| `multiply_112d_x87500` | 0.994 [0.983,1.006] |
| `multiply_1125d_x8750` | 0.993 [0.983,0.997] |
| `multiply_11250d_x875` | 0.991 [0.989,0.994] |
| `divide_scale112_x87500` | 1.005 [1.003,1.009] |
| `divide_scale1125_x8750` | 0.989 [0.979,1.016] |
| `divide_scale5625_x4375` | 0.992 [0.945,1.009] |
| `sqrt_scale112_x4375` | 0.996 [0.986,1.005] |
| `sqrt_scale1125_x44` | 1.011 [0.997,1.035] |
| `sqrt_scale5625_x5` | 1.001 [0.995,1.007] |
| `multiply_dense_1125d_x4375` | 0.997 [0.988,1.008] |
| `multiply_dense_11250d_x438` | 0.998 [0.986,1.004] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.002 [1.000,1.016] |
| `multiply_sparse_11250d_x875` | 0.996 [0.992,1.003] |
| `divide_multilimb_1012d_506d_s112_x875` | 0.969 [0.924,1.013] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.988 [0.958,1.004] |
| `modulo_1125d_393d_x2625` | 1.033 [0.930,1.092] |
| `power_112d_e17_x875` | 1.002 [0.979,1.011] |
| `power_22d_e32_x1750` | 0.976 [0.933,1.047] |
| `ibase_literal_flip_17500` | 0.995 [0.964,1.019] |
| `recursion_56_x875` | 0.995 [0.973,1.015] |
| `sparse_array_175000` | 1.016 [0.996,1.033] |
| `frontend_definitions_1750` | 1.028 [1.009,1.044] |
| `decimal_input_4500d_x88` | 0.999 [0.994,1.016] |
| `decimal_output_225d_x875` | 1.002 [0.953,1.234] |
| `streaming_statements_17500` | 1.009 [0.986,1.044] |
| `square_112d_x87500` | 1.003 [0.991,1.014] |
| `square_1125d_x8750` | 1.004 [0.996,1.006] |
| `square_11250d_x438` | 1.002 [0.991,1.007] |
| `literal_repeat_2250d_x1750` | 0.919 [0.861,0.987] |
| `binary_output_1125d_x263` | 1.018 [0.997,1.034] |
| `octal_output_1125d_x263` | 1.012 [1.000,1.090] |
| `binary_input_112500d_x3` | 0.999 [0.996,1.014] |
| `hex_input_112500d_x3` | 1.006 [0.989,1.009] |
| `temporary_chain_1125d_x43750` | 0.875 [0.865,0.877] |
