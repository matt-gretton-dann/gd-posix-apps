# Accepted fractional sqrt estimate with integrated decimal kernel

Refs #212, #200 and #197. PR #355 merged as `95f914a` after all fourteen
checks passed. Exact candidate `aff408d22dbf4284bf6db586271bc663e8bbd8e4`.
Local baseline is actual `7dc777eecd0abcc36d4381875253b031267d7d45`;
its tracked tree is identical to native baseline `8ab320a` (proof retained).
Do not relabel the local recipe as a different source revision. Both include
the accepted forward decimal kernel; earlier failed experiments remain under
`initial` and `expanded-native`. Their compiler mechanism remains unproven.

## Validation and measurement scope

Integrated Release passes217 cases/693324 assertions and six CLI/oracle groups.
Final native ON/OFF builds pass. Earlier separate profiling and actual OFF
ASan/UBSan observations stay in the initial archive with their real sources.
No new instrumentation or relaxed oracle is used for final timing.

Both full and held-out matrices check1800 invocations: fifty cases, two
roles, three warmups and fifteen shuffled samples. Schedule seeds1972121/
1972122 differ from corpus seeds197200/197201. The actual bounded corpus
uses one repeat for the largest tiny root, unlike the initial x10 experiment.
No local build, test, profile or RSS collection overlaps release timings.
Host frequency, thermals and scheduling remain uncontrolled.

No workload CI lies wholly above1.05. Decimal ratios1.00139 [0.99939,1.00714]
and0.99797 [0.99309,1.01404] are near parity. Tiny1024/1152 ratios0.01260/
0.01043 and tiny4096/4608 ratios0.00563/0.00426 retain the large improvement.
Every interval follows below; small roots, huge integers and large-scale
roots are retained independently rather than combined into one speedup.

All300 separate RSS executions pass. Tiny4096 median process peaks are
2457600/2080768 bytes; decimal input2326528/2342912 bytes. Peaks include
loader/frontend/VM/values/output/allocator. Three observations per role/case
do not establish portable memory bounds; all costs and raw logs are retained.

## Native observations

Linux/macOS/Windows each verify2000 invocations (6000 total), with no
confirmed regression. Decimal ratios are1.001 [0.931,1.016],
1.055 [0.953,1.169] (noisy/inconclusive), and0.987 [0.964,1.008].
Tiny4096 ratios are approximately0.004/0.006/0.006. These observations
support this integrated source, not the rejected earlier candidates.
All three raw ZIPs, source identities, schedules, A/A and class decisions
are preserved; workflow success alone is not the performance audit.

Precision-growth and capacity-reuse experiments are still outstanding
under issue #212. This archive completes the initial-estimate experiment.

## full intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.04702 [0.93683,1.06638] |
| `loop_1000000` | 1.00121 [0.99349,1.00420] |
| `calls_200000` | 0.99862 [0.98995,1.01284] |
| `array_200000` | 0.99691 [0.99107,1.00217] |
| `compare_high_1000d_x100000` | 1.00918 [0.99059,1.03112] |
| `compare_low_1000d_x100000` | 0.99687 [0.97956,1.00535] |
| `compare_equal_1000d_x100000` | 1.00220 [0.98647,1.01419] |
| `fibonacci_1000_x100` | 0.99940 [0.99778,1.00260] |
| `fibonacci_10000_x10` | 1.00303 [0.98511,1.00694] |
| `factorial_1000_x50` | 0.99928 [0.99126,1.00129] |
| `factorial_5000_x5` | 0.99733 [0.98985,1.00147] |
| `multiply_100d_x100000` | 0.99899 [0.99478,1.00658] |
| `multiply_1000d_x10000` | 1.00561 [0.99709,1.01147] |
| `multiply_10000d_x1000` | 1.00272 [0.99752,1.00600] |
| `divide_scale100_x100000` | 1.00164 [0.99906,1.00635] |
| `divide_scale1000_x10000` | 1.00301 [0.99709,1.03567] |
| `divide_scale5000_x5000` | 0.99984 [0.99497,1.01218] |
| `sqrt_scale100_x5000` | 0.99779 [0.99499,1.00244] |
| `sqrt_scale1000_x50` | 1.00491 [0.99920,1.01604] |
| `sqrt_scale5000_x5` | 1.00057 [0.99831,1.00461] |
| `multiply_dense_1000d_x5000` | 0.99751 [0.98200,1.00018] |
| `multiply_dense_10000d_x500` | 1.00380 [1.00005,1.00615] |
| `multiply_unbalanced_10000d_100d_x2000` | 0.98772 [0.98007,0.99937] |
| `multiply_sparse_10000d_x1000` | 1.00297 [1.00148,1.00539] |
| `divide_multilimb_900d_450d_s100_x1000` | 1.01696 [0.97938,1.05036] |
| `divide_multilimb_1000d_900d_s1000_x500` | 0.99046 [0.97713,1.00019] |
| `modulo_1000d_350d_x3000` | 0.99990 [0.98622,1.00833] |
| `power_100d_e17_x1000` | 1.00672 [0.99896,1.00908] |
| `power_20d_e32_x2000` | 1.00432 [0.98571,1.00941] |
| `ibase_literal_flip_20000` | 0.99927 [0.98369,1.00779] |
| `recursion_50_x1000` | 1.00086 [0.99251,1.01332] |
| `sparse_array_200000` | 0.99900 [0.98932,1.01989] |
| `frontend_definitions_2000` | 1.00658 [1.00581,1.01035] |
| `decimal_input_4000d_x100` | 1.00139 [0.99939,1.00714] |
| `decimal_output_200d_x1000` | 1.02281 [0.94262,1.06619] |
| `streaming_statements_20000` | 1.01453 [1.01096,1.01786] |
| `square_100d_x100000` | 0.99872 [0.99496,1.00379] |
| `square_1000d_x10000` | 1.00080 [0.99834,1.00362] |
| `square_10000d_x500` | 1.00085 [0.99145,1.00305] |
| `literal_repeat_2000d_x2000` | 1.02207 [0.97693,1.04497] |
| `binary_output_1000d_x300` | 1.02233 [0.96700,1.02934] |
| `octal_output_1000d_x300` | 1.00067 [0.99456,1.01190] |
| `binary_input_100000d_x3` | 0.99768 [0.98251,1.00822] |
| `hex_input_100000d_x3` | 1.00257 [0.99831,1.00618] |
| `temporary_chain_1000d_x50000` | 1.00124 [0.99582,1.00340] |
| `file_statements_20000` | 1.01051 [1.00795,1.02121] |
| `sqrt_large_scale10000_x5` | 1.00128 [0.99881,1.00223] |
| `sqrt_tiny_scale1024_x30` | 0.01260 [0.01178,0.01311] |
| `sqrt_tiny_scale4096_x1` | 0.00563 [0.00518,0.00569] |
| `sqrt_integer_power2000_scale100_x10` | 1.00060 [0.99706,1.00352] |

## holdout intervals

| Workload | Candidate / baseline, CI95 |
| --- | --- |
| `startup` | 1.01223 [0.96302,1.05416] |
| `loop_875000` | 1.00029 [0.99834,1.00402] |
| `calls_175000` | 1.00032 [0.99702,1.00592] |
| `array_175000` | 0.99852 [0.99303,1.00280] |
| `compare_high_1125d_x87500` | 0.99808 [0.97633,1.00047] |
| `compare_low_1125d_x87500` | 0.99104 [0.98322,1.00303] |
| `compare_equal_1125d_x87500` | 1.00103 [0.99035,1.01811] |
| `fibonacci_1125_x88` | 1.00377 [0.99982,1.00649] |
| `fibonacci_11250_x9` | 0.99942 [0.99512,1.00369] |
| `factorial_1125_x44` | 0.99586 [0.99029,0.99824] |
| `factorial_5625_x5` | 0.99872 [0.99360,1.00074] |
| `multiply_112d_x87500` | 0.99964 [0.99776,1.01623] |
| `multiply_1125d_x8750` | 1.00077 [0.99725,1.00338] |
| `multiply_11250d_x875` | 1.01106 [1.00757,1.01291] |
| `divide_scale112_x87500` | 0.99992 [0.99487,1.00882] |
| `divide_scale1125_x8750` | 0.99322 [0.98430,1.00836] |
| `divide_scale5625_x4375` | 0.99982 [0.99409,1.00187] |
| `sqrt_scale112_x4375` | 0.99710 [0.99506,1.00385] |
| `sqrt_scale1125_x44` | 1.00407 [0.99985,1.00790] |
| `sqrt_scale5625_x5` | 1.00268 [0.99676,1.00377] |
| `multiply_dense_1125d_x4375` | 1.00840 [1.00312,1.01303] |
| `multiply_dense_11250d_x438` | 0.99976 [0.99765,1.00490] |
| `multiply_unbalanced_11250d_112d_x1750` | 1.00487 [0.99961,1.01060] |
| `multiply_sparse_11250d_x875` | 1.00336 [1.00146,1.00496] |
| `divide_multilimb_1012d_506d_s112_x875` | 1.04079 [0.96409,1.06436] |
| `divide_multilimb_1125d_1012d_s1125_x438` | 1.01252 [0.99950,1.02222] |
| `modulo_1125d_393d_x2625` | 1.00854 [0.97525,1.06562] |
| `power_112d_e17_x875` | 1.01187 [1.00310,1.02664] |
| `power_22d_e32_x1750` | 1.00697 [0.99996,1.02701] |
| `ibase_literal_flip_17500` | 1.01453 [0.99892,1.02916] |
| `recursion_56_x875` | 0.99629 [0.99407,1.00165] |
| `sparse_array_175000` | 1.00026 [0.98664,1.00726] |
| `frontend_definitions_1750` | 1.00078 [0.99296,1.01999] |
| `decimal_input_4500d_x88` | 0.99797 [0.99309,1.01404] |
| `decimal_output_225d_x875` | 0.99325 [0.97263,1.01622] |
| `streaming_statements_17500` | 1.01735 [1.00891,1.02418] |
| `square_112d_x87500` | 0.99707 [0.99179,1.00293] |
| `square_1125d_x8750` | 1.00048 [0.99675,1.00217] |
| `square_11250d_x438` | 1.00131 [0.99954,1.00274] |
| `literal_repeat_2250d_x1750` | 0.99934 [0.95017,1.11222] |
| `binary_output_1125d_x263` | 1.01717 [0.98203,1.04187] |
| `octal_output_1125d_x263` | 1.01069 [0.99463,1.02131] |
| `binary_input_112500d_x3` | 1.00029 [0.99309,1.00152] |
| `hex_input_112500d_x3` | 0.99967 [0.99782,1.00162] |
| `temporary_chain_1125d_x43750` | 0.99857 [0.99607,1.00197] |
| `file_statements_17500` | 1.01569 [1.01072,1.02182] |
| `sqrt_large_scale11250_x5` | 1.00251 [0.99741,1.00849] |
| `sqrt_tiny_scale1152_x27` | 0.01043 [0.01010,0.01094] |
| `sqrt_tiny_scale4608_x1` | 0.00426 [0.00399,0.00446] |
| `sqrt_integer_power2250_scale112_x9` | 1.00097 [0.99804,1.00385] |
