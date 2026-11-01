# Initial fractional sqrt estimate: held candidate

Refs #212, #200 and #197. Exact candidate
`444e53d4a0068aeabda73f5615b3e2f40935f752`; matched local release and
diagnostic baseline `cc3d4ace9e25932325094e3a90da56e5537beba8`.
Native baseline is actual `1c11de3f68691d648389beb2cdd07d251ef6d638`.
Do not substitute these source identities. The native baseline does not include
the new diagnostic events; ordinary builds discard them.

## Correctness and diagnostic attribution

Release passes 217 cases/692100 assertions and four CLI/oracle groups; separate
profiling checks pass all sixteen CLI groups plus allocator/disabled tests.
Actual extensions-OFF ASan/UBSan passes seven sqrt cases/180 assertions and
four CLI/oracle groups. The new guess handles negative decimal exponents using
unsigned differences and preserves exact floor iteration and guard fallback.

Six diagnostic programs each contain exactly one sqrt and no other Number
division. At input scale 4096 the matched baseline takes 6815 Newton divisions,
candidate 14; scale 1024 is 1711/12. Sqrt2 at scale 100 stays 9 and sqrt4 at
scale 10000 stays 16. Full division dimensions and selected allocation peaks are
retained per case. These are diagnostic counts, not release time or process RSS.
The earlier pre-integration profile at 1926cbc remains preliminary and separate.

The #353 local sanitizer report initially used an unused extension option and
therefore ran ON. Its original log is retained as `matrix-asan-tests.log`.
The corrected `matrix-off-asan-*` logs verify ENABLE_NON_POSIX_EXTENSIONS=OFF
and pass lifetime plus all eight oracle groups. Pre-merge OFF CI was genuine.
Do not relabel the original ON observation as OFF.

## Matched local timing and separate RSS

Apple M1/macOS 27.2, Apple Clang 21/libc++ 220106, Release thin LTO, split 126,
extensions ON. Both presets check 1800 invocations: fifty cases, two roles,
three warmups and fifteen shuffled paired samples. All 3600 exact checks pass.
Schedule seeds 197212/197213 differ from corpus seeds 197200/197201. No class
has CI95 wholly above 1.05. No builds/tests/profiles/RSS overlap timings.
Host scheduling, frequency and thermals remain uncontrolled.

The actual harness is d6eb63c, with the original ten-repeat largest tiny case.
Its source hash is verified against both timing manifests. Later reduction to
one repeat for the Windows Howard deadline is a different workload. Do not
relabel these observations. Existing full/held-out source/oracle programs stay
in their artifacts, alongside every interval below.

Tiny ratios are 0.0118/0.0105 (scale 1024/1152) and 0.00276/0.00241
(scale 4096/4608). Other sqrt classes, including scale 10000/11250 and huge
integers, stay near parity. These gains do not excuse unrelated native costs.

All 300 separate fresh-process RSS checks pass, three per full case/role.
Tiny scale 4096 sampled medians are 2539520/2113536 bytes; scale 1024
2375680/2080768. Scale10000 medians are 2113536/2129920 and huge-integer
2179072/2195456. Costs remain visible; three samples do not establish portable
memory bounds. Peaks include loader/frontend/VM/values/output/allocator, not
isolated Newton storage; selected profile bytes are a separate quantity.

## Failed native acceptance

Digest/source audits verify 1780 Linux, 1750 macOS and 1780 Windows checked
invocations, 5310 total. Linux decimal input is a confirmed regression:
1.146 [1.09707,1.15332], bounded confirmation 1.120 [1.11456,1.12771].
A/A is 1.000 [0.97778,1.01559]. That workload does not execute sqrt. The
mechanism remains unproven; this is an acceptance failure, not sampling noise.
MacOS is noisy and Windows has no confirmed regression. The native harness
still selected only the small sqrt representative; #356 expands the regimes.

Thirteen ordinary checks pass. One duplicate push CodeQL C++ job fails before
analysis when Firefox snap installation receives HTTP408; PR CodeQL C++ passes.
Regardless, do not adopt this source with the confirmed Linux timing failure.
Preserve this complete dataset before evaluating any changed compiler policy;
never rerun unchanged code until favorable.

## full intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 0.99554 [0.96108,1.20446] |
| `loop_1000000` | 0.99273 [0.98746,0.99907] |
| `calls_200000` | 0.99356 [0.98285,0.99881] |
| `array_200000` | 0.99673 [0.99083,0.99863] |
| `compare_high_1000d_x100000` | 0.99667 [0.98030,1.02611] |
| `compare_low_1000d_x100000` | 1.00622 [1.00051,1.02506] |
| `compare_equal_1000d_x100000` | 0.98802 [0.98093,0.99432] |
| `fibonacci_1000_x100` | 1.00138 [0.99199,1.00530] |
| `fibonacci_10000_x10` | 0.99872 [0.99615,0.99962] |
| `factorial_1000_x50` | 0.99939 [0.99814,1.00300] |
| `factorial_5000_x5` | 1.00010 [0.99916,1.00204] |
| `multiply_100d_x100000` | 0.98942 [0.98812,0.99693] |
| `multiply_1000d_x10000` | 0.99886 [0.99802,1.00084] |
| `multiply_10000d_x1000` | 0.99500 [0.99212,0.99902] |
| `divide_scale 100_x100000` | 0.99864 [0.99513,1.00096] |
| `divide_scale 1000_x10000` | 1.00157 [0.99028,1.01083] |
| `divide_scale5000_x5000` | 1.00322 [1.00127,1.00481] |
| `sqrt_scale 100_x5000` | 1.00030 [0.99629,1.00337] |
| `sqrt_scale 1000_x50` | 1.00147 [0.99649,1.00614] |
| `sqrt_scale5000_x5` | 1.00067 [0.99352,1.00288] |
| `multiply_dense_1000d_x5000` | 1.00785 [1.00120,1.00938] |
| `multiply_dense_10000d_x500` | 1.00211 [0.99922,1.00441] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.99553 [0.98609,1.00095] |
| `multiply_sparse_10000d_x1000` | 1.00004 [0.99611,1.00141] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.03230 [0.99972,1.06316] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.99277 [0.97634,1.02231] |
| `modulo_1000d_350d_x3000` | 0.99141 [0.93859,1.00057] |
| `power_100d_e17_x1000` | 0.99898 [0.99678,1.00745] |
| `power_20d_e32_x2000` | 1.00503 [0.99528,1.01571] |
| `ibase_literal_flip_20000` | 1.00386 [0.99122,1.03217] |
| `recursion_50_x1000` | 0.99887 [0.99534,1.00424] |
| `sparse_array_200000` | 1.00180 [0.98853,1.01435] |
| `frontend_definitions_2000` | 0.99244 [0.98402,1.00181] |
| `decimal_input_4000d_x100` | 1.00012 [0.99439,1.00468] |
| `decimal_output_200d_x1000` | 1.00398 [0.96694,1.01544] |
| `streaming_statements_20000` | 1.00144 [0.99499,1.00397] |
| `square_100d_x100000` | 1.00160 [0.99590,1.00210] |
| `square_1000d_x10000` | 1.00112 [0.99801,1.00301] |
| `square_10000d_x500` | 0.99803 [0.99577,1.00352] |
| `literal_repeat_2000d_x2000` | 1.00626 [0.98342,1.14456] |
| `binary_output_1000d_x300` | 1.01567 [0.97499,1.03350] |
| `octal_output_1000d_x300` | 0.98854 [0.95494,1.00076] |
| `binary_input_100000d_x3` | 1.00568 [0.99865,1.01045] |
| `hex_input_100000d_x3` | 0.99985 [0.99604,1.00136] |
| `temporary_chain_1000d_x50000` | 0.99958 [0.99650,1.00172] |
| `file_statements_20000` | 1.00650 [1.00053,1.01449] |
| `sqrt_large_scale 10000_x5` | 0.99838 [0.99704,1.00118] |
| `sqrt_tiny_scale 1024_x30` | 0.01180 [0.01161,0.01303] |
| `sqrt_tiny_scale 4096_x10` | 0.00276 [0.00264,0.00282] |
| `sqrt_integer_power2000_scale 100_x10` | 0.99945 [0.99527,1.00353] |

## holdout intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.00269 [0.95403,1.04498] |
| `loop_875000` | 0.99397 [0.99274,0.99595] |
| `calls_175000` | 0.99453 [0.98997,1.00227] |
| `array_175000` | 0.98401 [0.98179,0.99703] |
| `compare_high_1125d_x87500` | 0.99424 [0.99071,1.02286] |
| `compare_low_1125d_x87500` | 0.98446 [0.97517,1.00277] |
| `compare_equal_1125d_x87500` | 1.00101 [0.97766,1.01564] |
| `fibonacci_1125_x88` | 1.00199 [0.99311,1.00367] |
| `fibonacci_11250_x9` | 1.00105 [0.99855,1.00671] |
| `factorial_1125_x44` | 0.99759 [0.98890,1.00096] |
| `factorial_5625_x5` | 0.99702 [0.99277,1.00127] |
| `multiply_112d_x87500` | 0.99481 [0.99329,0.99725] |
| `multiply_1125d_x8750` | 0.99982 [0.98602,1.00895] |
| `multiply_11250d_x875` | 0.98982 [0.98684,0.99530] |
| `divide_scale112_x87500` | 0.99631 [0.98116,0.99764] |
| `divide_scale1125_x8750` | 1.00348 [0.99884,1.01909] |
| `divide_scale5625_x4375` | 1.00037 [0.98985,1.01697] |
| `sqrt_scale112_x4375` | 0.99940 [0.99293,1.00118] |
| `sqrt_scale1125_x44` | 1.00196 [0.99678,1.00490] |
| `sqrt_scale5625_x5` | 1.00006 [0.99605,1.00349] |
| `multiply_dense_1125d_x4375` | 1.00064 [0.99807,1.00655] |
| `multiply_dense_11250d_x438` | 0.99806 [0.99512,1.00131] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.00247 [0.99812,1.00445] |
| `multiply_sparse_11250d_x875` | 0.99869 [0.99068,0.99974] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.00113 [0.96285,1.03925] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 0.99131 [0.98052,1.00183] |
| `modulo_1125d_393d_x2625` | 0.98392 [0.92155,1.02788] |
| `power_112d_e17_x875` | 1.00066 [0.98761,1.01402] |
| `power_22d_e32_x1750` | 0.99079 [0.97142,1.02328] |
| `ibase_literal_flip_17500` | 0.99740 [0.98834,1.00450] |
| `recursion_56_x875` | 0.99653 [0.98523,1.00770] |
| `sparse_array_175000` | 1.00445 [0.97275,1.01295] |
| `frontend_definitions_1750` | 1.00677 [0.99906,1.02596] |
| `decimal_input_4500d_x88` | 1.00068 [0.99672,1.00633] |
| `decimal_output_225d_x875` | 0.99157 [0.97545,1.00262] |
| `streaming_statements_17500` | 1.00357 [0.99704,1.00542] |
| `square_112d_x87500` | 0.99881 [0.99314,1.00237] |
| `square_1125d_x8750` | 1.00127 [0.99643,1.00335] |
| `square_11250d_x438` | 1.00030 [0.99844,1.00279] |
| `literal_repeat_2250d_x1750` | 1.01962 [1.01072,1.03805] |
| `binary_output_1125d_x263` | 1.00062 [0.97017,1.00957] |
| `octal_output_1125d_x263` | 0.99117 [0.98264,1.01194] |
| `binary_input_112500d_x3` | 0.99877 [0.99382,1.00412] |
| `hex_input_112500d_x3` | 0.99781 [0.99640,1.00304] |
| `temporary_chain_1125d_x43750` | 1.00113 [0.99106,1.00489] |
| `file_statements_17500` | 1.00517 [1.00086,1.00950] |
| `sqrt_large_scale11250_x5` | 0.99860 [0.99787,1.00182] |
| `sqrt_tiny_scale1152_x27` | 0.01054 [0.01027,0.01144] |
| `sqrt_tiny_scale4608_x9` | 0.00241 [0.00234,0.00252] |
| `sqrt_integer_power2250_scale112_x9` | 0.99784 [0.99208,0.99928] |
