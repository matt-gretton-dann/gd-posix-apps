# bc native paired results

Performance decisions are advisory; correctness, builds and deadlines are hard failures.

Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.
A/A independently rebuilds the same base. Startup remains inconclusive until calibrated.

| Preset | Comparison | Workload | Ratio [95%] | Advisory status |
| --- | --- | --- | --- | --- |
| full | aa | `startup` | 0.984 [0.8800244231709766, 1.1366235966865372] | inconclusive-noisy |
| full | aa | `loop_1000000` | 1.090 [0.9959775797056364, 1.1391990652644008] | inconclusive-noisy |
| full | aa | `calls_200000` | 1.000 [0.996571437178166, 1.0052556155889214] | pass |
| full | aa | `array_200000` | 1.018 [0.9738760427312445, 1.0287340157950593] | pass |
| full | aa | `compare_high_1000d_x100000` | 0.999 [0.9894273784666587, 1.0066664441067519] | pass |
| full | aa | `compare_low_1000d_x100000` | 0.998 [0.9907197824341, 1.0075628420643055] | pass |
| full | aa | `compare_equal_1000d_x100000` | 1.003 [0.9967046921666007, 1.0071913019340901] | pass |
| full | aa | `fibonacci_1000_x100` | 1.001 [0.9590892489608177, 1.0121631882682858] | pass |
| full | aa | `multiply_100d_x100000` | 0.998 [0.9910911666343276, 1.0049189897923743] | pass |
| full | aa | `multiply_10000d_x1000` | 1.004 [0.9938297495206347, 1.0789378996979468] | inconclusive-noisy |
| full | aa | `sqrt_scale100_x5000` | 1.003 [0.9917722014827983, 1.0201562092576015] | pass |
| full | aa | `multiply_dense_10000d_x500` | 1.000 [0.9925072483231462, 1.0054746386511082] | pass |
| full | aa | `multiply_unbalanced_10000d_100d_x2000` | 1.002 [0.9909888975027116, 1.0077012989405785] | pass |
| full | aa | `multiply_sparse_10000d_x1000` | 1.001 [0.9976568771145871, 1.0099057660900026] | pass |
| full | aa | `divide_multilimb_900d_450d_s100_x1000` | 0.977 [0.9669982402314525, 1.0107318516031085] | pass |
| full | aa | `power_100d_e17_x1000` | 0.999 [0.9948963849031924, 1.0034478056551033] | pass |
| full | aa | `sparse_array_200000` | 1.002 [0.9863475816092405, 1.0095223079186646] | pass |
| full | aa | `decimal_input_4000d_x100` | 1.000 [0.9890321950264694, 1.0005495019308053] | pass |
| full | aa | `streaming_statements_20000` | 0.999 [0.9876731323512966, 1.006814632750849] | pass |
| full | paired | `startup` | 1.000 [0.9290860405860225, 1.1174793088996373] | inconclusive-noisy |
| full | paired | `loop_1000000` | 0.981 [0.8989531191367821, 0.9861317068251226] | pass |
| full | paired | `calls_200000` | 0.947 [0.9416711085329206, 0.9494579253573094] | pass |
| full | paired | `array_200000` | 1.007 [0.9915203948717737, 1.0265556656613954] | pass |
| full | paired | `compare_high_1000d_x100000` | 0.977 [0.9706515238258654, 0.9789648823516417] | pass |
| full | paired | `compare_low_1000d_x100000` | 0.974 [0.9665622780564692, 0.9781013079289615] | pass |
| full | paired | `compare_equal_1000d_x100000` | 0.989 [0.9861017990635195, 0.9959762113683557] | pass |
| full | paired | `fibonacci_1000_x100` | 0.987 [0.9758598211781756, 1.0068098614164074] | pass |
| full | paired | `multiply_100d_x100000` | 0.978 [0.9718106830756705, 0.9845086718955287] | pass |
| full | paired | `multiply_10000d_x1000` | 0.900 [0.8929226687015922, 0.9454829262337022] | pass |
| full | paired | `sqrt_scale100_x5000` | 0.989 [0.9781397610532644, 0.9963974786525699] | pass |
| full | paired | `multiply_dense_10000d_x500` | 0.844 [0.7962592581756113, 0.8461222616484317] | pass |
| full | paired | `multiply_unbalanced_10000d_100d_x2000` | 0.997 [0.9808667912200832, 1.0013255930706333] | pass |
| full | paired | `multiply_sparse_10000d_x1000` | 0.368 [0.3545906208081092, 0.36993230883683453] | pass |
| full | paired | `divide_multilimb_900d_450d_s100_x1000` | 1.057 [0.9948198087656968, 1.0713449560698767] | inconclusive-noisy |
| full | paired | `power_100d_e17_x1000` | 1.002 [0.9743384625301227, 1.0052126921435114] | pass |
| full | paired | `sparse_array_200000` | 0.957 [0.9276533895272373, 0.9611574762280775] | pass |
| full | paired | `decimal_input_4000d_x100` | 0.997 [0.9962184402790404, 1.0027376595930941] | pass |
| full | paired | `streaming_statements_20000` | 1.022 [0.9933362897481571, 1.0440041102474988] | pass |
