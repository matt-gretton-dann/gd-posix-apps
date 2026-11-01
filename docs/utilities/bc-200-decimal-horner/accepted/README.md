# Forward decimal chunks: accepted generic kernel

Refs #200, #206, #212 and #197. PR #357 accepts candidate
`7dc777eecd0abcc36d4381875253b031267d7d45` against baseline
`10d8a2726fef861925f9178d89d44fbe3ca061de`. Merge is
`8ab320a39ba1622f88edd0bc492822c8a83da22c`; local main was immediately
fast-forwarded. Exact immutable recipes retain source/compiler/library/flags,
binary hashes, fixed split 126 and extensions ON. Local Apple M1/macOS 27.2,
Apple Clang 21/libc++ 220106, Release thin LTO. No diagnostic instrumentation
contributes to speed samples.

## Algorithm and correctness

Decode the partial leading decimal group and then full groups forward with
ordinary limb multiply-by-ten arithmetic. Emit completed groups into the
pre-sized little-endian vector from the back. Each prefix is below the limb
base, so the separate positional weight and wide multiply are unnecessary.
Zero/single-digit behavior, decimal scale, fractional trailing coefficient
zeros and shared snapshots are unchanged. The same generic path covers all
supported traits; no compiler hints or architecture-specific decoder is added.

Test-first independent arithmetic covers every point position around one/two
limb boundaries, all-nine maximal groups and zero suffixes. The old kernel
passes 2460 assertions in three decimal-oracle cases. New Release passes 214
cases/693228 assertions and five CLI/oracle groups; actual extensions-OFF
ASan/UBSan passes those three cases and the same five groups. Integration
also passes 23 harness groups with 50 actual quick oracles, six native-policy
groups and 2981 existing payload hashes/eleven checkout simulations.

## Matched local timing and separate memory

Both presets contain 1800 checked invocations: 50 cases, two roles, three warmups
and fifteen shuffled paired samples. All 3600 exact output/status/stderr/deadline
checks pass. Schedule seeds 1972001/1972002 differ from corpus seeds 197200/197201.
The retained harness hash matches both manifests. All intervals and costs appear
below; no class has CI95 wholly above 1.05. No builds, tests, profiles or RSS
collection overlap these timings. Host scheduling/frequency/thermals are uncontrolled.

Decimal input ratios are 0.997 [0.994,1.000] full and 0.997 [0.993,0.999] held-out.
This is near parity locally. All 300 separate fresh-process RSS checks pass, three
per full case/role; decimal-input median peaks are both 2293760 bytes. Peaks
include loader/frontend/VM/values/output/allocator and do not isolate decoder
storage. Three samples do not establish portable memory bounds or equivalence.

## Native acceptance and limitations

All fourteen checks pass. Three digest/source-verified native archives retain
2000 checked invocations each, 6000 total, with no confirmed regression.
Decimal input ratios: Linux 0.984 [0.973,1.000], macOS 0.946 [0.834,0.952],
Windows 0.986 [0.888,1.016]. Other noisy/costly observations remain in full
summaries; for example Linux equality 1.014 [1.003,1.127] is inconclusive.
Do not claim a large portable speedup from these modest/noisy outcomes.

Adopt the smaller generic kernel on its exact checks and scoped evidence.
This does not establish the compiler mechanism of the earlier #355 Linux
decimal-input loss, nor resolve that candidate. The fractional estimate is unaccepted; its native failures remain visible.
Any resumed candidate needs its own integrated native acceptance. #200 competitive targets and
#197 final coverage/acceptance remain open.

## full paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 0.94717 [0.89941,1.06838] |
| `loop_1000000` | 1.00506 [1.00194,1.00580] |
| `calls_200000` | 1.00112 [0.99614,1.00937] |
| `array_200000` | 0.99573 [0.98746,1.00126] |
| `compare_high_1000d_x100000` | 1.00034 [0.99367,1.00258] |
| `compare_low_1000d_x100000` | 0.99603 [0.99307,1.00671] |
| `compare_equal_1000d_x100000` | 1.00953 [0.99852,1.01358] |
| `fibonacci_1000_x100` | 0.99852 [0.99410,1.00253] |
| `fibonacci_10000_x10` | 1.00095 [0.99894,1.00270] |
| `factorial_1000_x50` | 0.99953 [0.99656,1.00312] |
| `factorial_5000_x5` | 0.99940 [0.99290,1.00353] |
| `multiply_100d_x100000` | 0.99934 [0.99655,1.01433] |
| `multiply_1000d_x10000` | 0.99989 [0.99825,1.00549] |
| `multiply_10000d_x1000` | 0.99893 [0.99703,1.00214] |
| `divide_scale100_x100000` | 0.99970 [0.99422,1.00246] |
| `divide_scale1000_x10000` | 0.99102 [0.96530,0.99772] |
| `divide_scale5000_x5000` | 0.99968 [0.99794,1.00707] |
| `sqrt_scale100_x5000` | 0.99364 [0.99255,1.00378] |
| `sqrt_scale1000_x50` | 0.99927 [0.98540,1.00191] |
| `sqrt_scale5000_x5` | 1.00042 [0.99624,1.00584] |
| `multiply_dense_1000d_x5000` | 1.00101 [0.99897,1.01447] |
| `multiply_dense_10000d_x500` | 1.00127 [1.00007,1.00526] |
| `multiply_unbalanced_10000d_100d_x2000` | 1.00234 [0.99458,1.00523] |
| `multiply_sparse_10000d_x1000` | 0.99950 [0.99688,1.00260] |
| `divide_multilimb_900d_450d_s100_x1000` | 0.97903 [0.96917,1.03170] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.99990 [0.99694,1.00786] |
| `modulo_1000d_350d_x3000` | 1.01120 [0.97777,1.02020] |
| `power_100d_e17_x1000` | 1.00076 [0.99600,1.01849] |
| `power_20d_e32_x2000` | 0.98477 [0.97914,1.00362] |
| `ibase_literal_flip_20000` | 0.99010 [0.98071,1.00781] |
| `recursion_50_x1000` | 1.00745 [0.99532,1.01565] |
| `sparse_array_200000` | 1.00014 [0.99766,1.00641] |
| `frontend_definitions_2000` | 0.99132 [0.97274,0.99678] |
| `decimal_input_4000d_x100` | 0.99709 [0.99407,1.00016] |
| `decimal_output_200d_x1000` | 0.98294 [0.93747,1.02712] |
| `streaming_statements_20000` | 0.99374 [0.98720,0.99475] |
| `square_100d_x100000` | 0.99965 [0.98374,1.00582] |
| `square_1000d_x10000` | 0.99958 [0.99939,1.00236] |
| `square_10000d_x500` | 0.99745 [0.99601,1.00105] |
| `literal_repeat_2000d_x2000` | 1.00868 [0.98610,1.04738] |
| `binary_output_1000d_x300` | 1.01595 [0.98570,1.02863] |
| `octal_output_1000d_x300` | 0.99051 [0.98954,1.00635] |
| `binary_input_100000d_x3` | 0.99525 [0.99353,1.00002] |
| `hex_input_100000d_x3` | 0.99913 [0.99477,1.00061] |
| `temporary_chain_1000d_x50000` | 0.99818 [0.99130,1.00070] |
| `file_statements_20000` | 0.99386 [0.98135,1.00107] |
| `sqrt_large_scale10000_x5` | 1.00165 [0.99781,1.00496] |
| `sqrt_tiny_scale1024_x30` | 0.99843 [0.99671,1.00239] |
| `sqrt_tiny_scale4096_x1` | 1.00013 [0.99968,1.00096] |
| `sqrt_integer_power2000_scale100_x10` | 1.00273 [0.99813,1.00548] |

## holdout paired intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.03472 [0.98100,1.11501] |
| `loop_875000` | 1.00144 [0.99791,1.00257] |
| `calls_175000` | 0.99999 [0.99284,1.00892] |
| `array_175000` | 0.99381 [0.98836,0.99840] |
| `compare_high_1125d_x87500` | 0.99418 [0.98201,0.99760] |
| `compare_low_1125d_x87500` | 1.01128 [0.98996,1.02262] |
| `compare_equal_1125d_x87500` | 0.99766 [0.98554,1.01384] |
| `fibonacci_1125_x88` | 1.00128 [0.99598,1.00950] |
| `fibonacci_11250_x9` | 0.99852 [0.99400,1.00305] |
| `factorial_1125_x44` | 0.99892 [0.99733,1.00196] |
| `factorial_5625_x5` | 1.00082 [0.99814,1.00475] |
| `multiply_112d_x87500` | 1.00170 [0.99713,1.00420] |
| `multiply_1125d_x8750` | 0.99642 [0.99374,1.00036] |
| `multiply_11250d_x875` | 0.99914 [0.99643,1.00334] |
| `divide_scale112_x87500` | 1.00170 [0.99723,1.02428] |
| `divide_scale1125_x8750` | 1.00003 [0.99287,1.03400] |
| `divide_scale5625_x4375` | 1.00180 [0.99410,1.01059] |
| `sqrt_scale112_x4375` | 1.00011 [0.99353,1.00436] |
| `sqrt_scale1125_x44` | 0.99908 [0.99678,1.00541] |
| `sqrt_scale5625_x5` | 1.00026 [0.99868,1.00280] |
| `multiply_dense_1125d_x4375` | 1.00044 [0.99532,1.00256] |
| `multiply_dense_11250d_x438` | 1.00056 [0.99828,1.00267] |
| `multiply_unbalanced_11250d_112d_x1750` | 0.99638 [0.99205,0.99875] |
| `multiply_sparse_11250d_x875` | 1.00050 [0.99725,1.00163] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.00492 [0.97872,1.03264] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.99971 [0.98963,1.01989] |
| `modulo_1125d_393d_x2625` | 0.99511 [0.97700,1.01293] |
| `power_112d_e17_x875` | 1.00333 [0.99988,1.03511] |
| `power_22d_e32_x1750` | 0.99802 [0.98372,1.00714] |
| `ibase_literal_flip_17500` | 0.98908 [0.97919,1.02245] |
| `recursion_56_x875` | 1.00378 [0.98813,1.01910] |
| `sparse_array_175000` | 0.99507 [0.98551,0.99903] |
| `frontend_definitions_1750` | 0.99587 [0.98378,1.00259] |
| `decimal_input_4500d_x88` | 0.99666 [0.99304,0.99943] |
| `decimal_output_225d_x875` | 0.99793 [0.98469,1.02323] |
| `streaming_statements_17500` | 0.98729 [0.97522,0.99602] |
| `square_112d_x87500` | 0.99925 [0.99742,1.00135] |
| `square_1125d_x8750` | 0.99965 [0.99851,1.00130] |
| `square_11250d_x438` | 1.00244 [0.99670,1.00309] |
| `literal_repeat_2250d_x1750` | 1.01959 [1.00829,1.11893] |
| `binary_output_1125d_x263` | 1.00662 [0.98757,1.03755] |
| `octal_output_1125d_x263` | 1.00546 [0.98721,1.01331] |
| `binary_input_112500d_x3` | 0.99529 [0.99010,1.00238] |
| `hex_input_112500d_x3` | 1.00128 [0.99708,1.00234] |
| `temporary_chain_1125d_x43750` | 0.99573 [0.98833,0.99777] |
| `file_statements_17500` | 0.98823 [0.98246,0.99193] |
| `sqrt_large_scale11250_x5` | 0.99988 [0.99741,1.00262] |
| `sqrt_tiny_scale1152_x27` | 1.00028 [0.99744,1.00105] |
| `sqrt_tiny_scale4608_x1` | 0.99936 [0.99751,1.00186] |
| `sqrt_integer_power2250_scale112_x9` | 1.00011 [0.99313,1.00837] |
