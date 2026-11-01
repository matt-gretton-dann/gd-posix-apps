# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.979 [0.944305393310263, 1.0367243164378035] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.001 [0.9889823775864195, 1.0042659828949403] | pass |
| full | aa | `calls_200000` | 1.008 [0.9896327868892182, 1.0121091059229332] | pass |
| full | aa | `array_200000` | 1.005 [0.9840007141369964, 1.0227116268052363] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.999 [0.9857408486856707, 1.0092484823943064] | pass |
| full | aa | `compare_low_1000d_x100000` | 1.003 [0.987244487085016, 1.0289521152519299] | pass |
| full | aa | `compare_equal_1000d_x100000` | 0.991 [0.9732527830485641, 1.0012174286149926] | pass |
| full | aa | `fibonacci_1000_x100` | 0.999 [0.9776877993426241, 1.01170142332025] | pass |
| full | aa | `multiply_100d_x100000` | 1.006 [0.998100096781084, 1.0239123247585697] | pass |
| full | aa | `multiply_10000d_x1000` | 1.000 [0.998777033256719, 1.0023911658880702] | pass |
| full | aa | `sqrt_scale100_x5000` | 0.996 [0.9914530573334492, 1.0069146772229056] | pass |
| full | aa | `multiply_dense_10000d_x500` | 0.999 [0.9983122114699705, 1.0000395721927229] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 0.999 [0.9983897317111247, 1.0013756530047107] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.000 [0.9986117168815436, 1.0019245704491626] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 1.004 [0.9969686128779572, 1.0522753958154722] | inconclusive-noisy |
| full | aa | `power_100d_e17_x1000` | 1.001 [0.9978264270566201, 1.0040732487553938] | pass |
| full | aa | `sparse_array_200000` | 1.004 [0.9882679444946892, 1.0223988140454157] | pass |
| full | aa | `decimal_input_4000d_x100` | 0.998 [0.9945891081763217, 0.99965142455697] | pass |
| full | aa | `streaming_statements_20000` | 0.996 [0.9850746080727164, 1.0050929596832596] | pass |
| full | paired | `startup` | 1.029 [0.9589524736560314, 1.1600460827858021] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.967 [0.9477322698282209, 1.0116671945126523] | pass |
| full | paired | `calls_200000` | 1.026 [1.0219376966969804, 1.03234439237834] | pass |
| full | paired | `array_200000` | 0.986 [0.9498725237002736, 1.002565987171858] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.970 [0.9653093836647645, 0.9853373844590659] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.992 [0.950200318685459, 1.016172994354558] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.997 [0.9803954190149125, 1.0383676213528452] | pass |
| full | paired | `fibonacci_1000_x100` | 0.979 [0.9660445165848782, 0.9920631501915641] | pass |
| full | paired | `multiply_100d_x100000` | 1.032 [1.0006543308073925, 1.0405282455874432] | pass |
| full | paired | `multiply_10000d_x1000` | 0.895 [0.8878347369732561, 0.8970665051498597] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.991 [0.985483003003435, 0.99314192487606] | pass |
| full | paired | `multiply_dense_10000d_x500` | 0.897 [0.8860726418372575, 0.8979163922336374] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.998 [0.99611551638101, 1.0001290620064291] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.357 [0.3556758730357602, 0.3579568454711976] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 1.000 [0.9875006146044802, 1.0334271541312539] | pass |
| full | paired | `power_100d_e17_x1000` | 0.996 [0.9902275788052464, 1.0000098886440707] | pass |
| full | paired | `sparse_array_200000` | 1.015 [0.9738862264053686, 1.0331042412985714] | pass |
| full | paired | `decimal_input_4000d_x100` | 0.994 [0.9853135236010963, 1.0090447515422283] | pass |
| full | paired | `streaming_statements_20000` | 1.004 [0.9997157261895401, 1.0144978792080763] | pass |
